package webjars

import com.augustnagro.magnum.sql
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import webjars.models.WebJarVersion
import webjars.utils.*
import zio.*
import zio.test.*

import java.time.{LocalDate, ZonedDateTime}
import javax.sql.DataSource

object PopularRankingSpec extends ZIOSpecDefault:

  private val classicGroup = MavenCentral.GroupId("org.webjars")
  private val npmGroup     = MavenCentral.GroupId("org.webjars.npm")

  private val testDeployables: AllDeployables = new AllDeployables:
    def fromGroupId(groupId: MavenCentral.GroupId) = None
    def fromName(name: String)                     = None
    def groupIds(): Set[MavenCentral.GroupId]      = Set(classicGroup, npmGroup)

  private def meta(name: String): WebJarsCache.WebJarMeta =
    WebJarsCache.WebJarMeta(name, s"https://example.com/$name", List(WebJarVersion("1.0.0", Some(1))), ZonedDateTime.now())

  private val seedArtifact: (MavenCentral.GroupId, MavenCentral.ArtifactId, String) => RIO[DataSource, Unit] =
    (gid, aid, name) =>
      WebJarsCache.setArtifactDetails(GroupArtifact(gid, aid), meta(name))

  private val todayLocal: UIO[LocalDate] = PopularMetrics.today

  private def addScore(day: LocalDate, artifactId: String, score: Long): RIO[DataSource, Unit] =
    PostgresContainer.execute(sql"insert into popular_daily values ($day, 'org.webjars', $artifactId, $score)").unit

  private val flushAll: RIO[DataSource, Unit] = PostgresContainer.truncateAll

  private def newRanking: UIO[PopularRanking] =
    Ref.make(List.empty[webjars.models.WebJar]).map(PopularRankingLive(_, testDeployables))

  def spec = suite("PopularRanking")(

    test("populate aggregates the 30-day window and hydrates the top members"):
      for
        _        <- flushAll
        today    <- todayLocal
        // Scores: jquery 100 today, react 50 today + 30 yesterday = 80 total.
        // jquery wins.
        _        <- addScore(today, "jquery", 100L)
        _        <- addScore(today, "react", 50L)
        _        <- addScore(today.minusDays(1), "react", 30L)
        _        <- seedArtifact(classicGroup, MavenCentral.ArtifactId("jquery"), "jQuery")
        _        <- seedArtifact(classicGroup, MavenCentral.ArtifactId("react"),  "React")
        ranking  <- newRanking
        _        <- ranking.populate
        snapshot <- ranking.snapshot
      yield assertTrue(
        snapshot.map(_.artifactId) == List("jquery", "react"),
        snapshot.head.name == "jQuery",
      ),

    test("populate drops members that don't hydrate from WebJarsCache"):
      for
        _        <- flushAll
        today    <- todayLocal
        _        <- addScore(today, "jquery", 200L)
        _        <- addScore(today, "ghost-artifact", 100L)
        _        <- seedArtifact(classicGroup, MavenCentral.ArtifactId("jquery"), "jQuery")
        ranking  <- newRanking
        _        <- ranking.populate
        snapshot <- ranking.snapshot
      yield assertTrue(
        snapshot.map(_.artifactId) == List("jquery"),
      ),

    test("populate falls back to the first artifacts per group when the window is empty"):
      for
        _        <- flushAll
        // No popularity scores at all; only cached artifacts exist.
        _        <- seedArtifact(classicGroup, MavenCentral.ArtifactId("jquery"), "jQuery")
        _        <- seedArtifact(npmGroup,     MavenCentral.ArtifactId("vue"),    "Vue")
        ranking  <- newRanking
        _        <- ranking.populate
        snapshot <- ranking.snapshot
      yield assertTrue(
        snapshot.map(_.artifactId).toSet == Set("jquery", "vue"),
      ),

    test("snapshot returns Nil before populate runs"):
      for
        _        <- flushAll
        ranking  <- newRanking
        snapshot <- ranking.snapshot
      yield assertTrue(snapshot.isEmpty),

    test("rebuild picks up updated WebJarsCache state (e.g. numFiles backfill)"):
      // Regression test for the home-page-staleness bug: PopularRanking's
      // snapshot is built once at boot and never refreshed without
      // `rebuild`. Without this, the periodic numFiles backfill would
      // update WebJarsCache but `/` would keep showing stale values
      // until the dyno cycled.
      for
        _              <- flushAll
        today          <- todayLocal
        _              <- addScore(today, "jquery", 100L)
        // Initially seed jquery with numFiles = None (the broken state
        // we're fixing).
        ga              = GroupArtifact(classicGroup, MavenCentral.ArtifactId("jquery"))
        initialMeta     = WebJarsCache.WebJarMeta("jQuery", "https://example.com/jquery",
                            List(WebJarVersion("3.7.1", None)), ZonedDateTime.now())
        _              <- WebJarsCache.setArtifactDetails(ga, initialMeta)
        ranking        <- newRanking
        _              <- ranking.populate
        beforeRebuild  <- ranking.snapshot
        // Simulate `refreshMissingNumFiles` filling in the value.
        backfilledMeta  = initialMeta.copy(versions = List(WebJarVersion("3.7.1", Some(1234))))
        _              <- WebJarsCache.setArtifactDetails(ga, backfilledMeta)
        // The snapshot still reflects the OLD state (None) — proving
        // populate's one-shot semantics are what they look like.
        stillStale     <- ranking.snapshot
        // rebuild re-aggregates from the current cache.
        _              <- ranking.rebuild
        afterRebuild   <- ranking.snapshot
      yield assertTrue(
        beforeRebuild.flatMap(_.versions.headOption).flatMap(_.numFiles).isEmpty,
        stillStale.flatMap(_.versions.headOption).flatMap(_.numFiles).isEmpty,
        afterRebuild.flatMap(_.versions.headOption).flatMap(_.numFiles).contains(1234),
      ),

    test("rebuild is a no-op when both the aggregate and fallback are empty"):
      // Defensive: a rebuild called against an empty database must not
      // wipe out the existing snapshot. Otherwise a transient database
      // blip could blank the home page.
      for
        _               <- flushAll
        _               <- seedArtifact(classicGroup, MavenCentral.ArtifactId("jquery"), "jQuery")
        ranking         <- newRanking
        _               <- ranking.populate
        beforeRebuild   <- ranking.snapshot
        _               <- flushAll  // wipe everything
        _               <- ranking.rebuild
        afterRebuild    <- ranking.snapshot
      yield assertTrue(
        beforeRebuild.map(_.artifactId) == List("jquery"),
        // snapshot is preserved, not wiped
        afterRebuild.map(_.artifactId) == List("jquery"),
      ),

  ).provide(PostgresContainer.freshDatabase) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
