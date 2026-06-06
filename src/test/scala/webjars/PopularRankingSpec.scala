package webjars

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import org.testcontainers.containers.wait.strategy.Wait
import webjars.models.WebJarVersion
import webjars.utils.*
import zio.*
import zio.redis.{CodecSupplier, Redis, RedisConfig}
import zio.test.*

import java.time.{ZoneOffset, ZonedDateTime}

object PopularRankingSpec extends ZIOSpecDefault:

  private val container: GenericContainer =
    val c = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort()
    )
    c.start()
    c

  private val redisLayer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ Valkey.codecSupplierLayer >>> Redis.singleNode.orDie

  private val classicGroup = MavenCentral.GroupId("org.webjars")
  private val npmGroup     = MavenCentral.GroupId("org.webjars.npm")

  private val testDeployables: AllDeployables = new AllDeployables:
    def fromGroupId(groupId: MavenCentral.GroupId) = None
    def fromName(name: String)                     = None
    def groupIds(): Set[MavenCentral.GroupId]      = Set(classicGroup, npmGroup)

  private def meta(name: String): WebJarsCache.WebJarMeta =
    WebJarsCache.WebJarMeta(name, s"https://example.com/$name", List(WebJarVersion("1.0.0", Some(1))), ZonedDateTime.now())

  private val seedArtifact: (MavenCentral.GroupId, MavenCentral.ArtifactId, String) => ZIO[Redis, Throwable, Unit] =
    (gid, aid, name) =>
      WebJarsCache.setArtifactDetails(GroupArtifact(gid, aid), meta(name))

  private val todayLocal: UIO[java.time.LocalDate] =
    Clock.currentDateTime.map(_.atZoneSameInstant(ZoneOffset.UTC).toLocalDate)

  private val flushAll: ZIO[Redis, Nothing, Unit] =
    ZIO.serviceWithZIO[Redis](_.flushall().orDie.unit)

  private def newRanking: UIO[PopularRanking] =
    Ref.make(List.empty[webjars.models.WebJar]).map(PopularRankingLive(_, testDeployables))

  def spec = suite("PopularRanking")(

    test("populate aggregates the 30-day window and hydrates the top members"):
      for
        _        <- flushAll
        today    <- todayLocal
        // ZSET scores: jquery 100 today, react 50 today + 30 yesterday = 80 total.
        // jquery wins.
        _        <- ZIO.serviceWithZIO[Redis]: redis =>
                      val keyToday = s"popular:$today"
                      val keyYday  = s"popular:${today.minusDays(1)}"
                      redis.zIncrBy(keyToday, 100L, "org.webjars:jquery") *>
                        redis.zIncrBy(keyToday, 50L,  "org.webjars:react") *>
                        redis.zIncrBy(keyYday,  30L,  "org.webjars:react")
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
        _        <- ZIO.serviceWithZIO[Redis]: redis =>
                      val k = s"popular:$today"
                      redis.zIncrBy(k, 200L, "org.webjars:jquery") *>
                        redis.zIncrBy(k, 100L, "org.webjars:ghost-artifact")
        _        <- seedArtifact(classicGroup, MavenCentral.ArtifactId("jquery"), "jQuery")
        ranking  <- newRanking
        _        <- ranking.populate
        snapshot <- ranking.snapshot
      yield assertTrue(
        snapshot.map(_.artifactId) == List("jquery"),
      ),

    test("populate falls back to per-group HGETALL when the window is empty"):
      for
        _        <- flushAll
        // No popular:<date> keys at all; only the cache hashes exist.
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
        _              <- ZIO.serviceWithZIO[Redis]: redis =>
                            redis.zIncrBy(s"popular:$today", 100L, "org.webjars:jquery").unit
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
      // Defensive: a rebuild called against an empty Redis must not
      // wipe out the existing snapshot. Otherwise a transient Redis
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

  ).provideLayer(redisLayer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
