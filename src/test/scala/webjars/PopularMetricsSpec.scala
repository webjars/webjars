package webjars

import com.augustnagro.magnum.*
import com.augustnagro.magnum.ziomagnum.*
import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.{Db, PopularMetrics, PopularMetricsLive}
import webjars.utils.Db.given
import zio.*
import zio.test.*

import java.time.LocalDate
import javax.sql.DataSource

object PopularMetricsSpec extends ZIOSpecDefault:

  private val popularMetrics: PopularMetrics = PopularMetricsLive()

  private val groupId   = MavenCentral.GroupId("org.webjars")
  private val npmGroup  = MavenCentral.GroupId("org.webjars.npm")
  private val jqueryId  = MavenCentral.ArtifactId("jquery")
  private val reactId   = MavenCentral.ArtifactId("react")
  private val githubId  = MavenCentral.ArtifactId("github-com-Sortable-Sortable")

  private def score(day: LocalDate, g: MavenCentral.GroupId, a: MavenCentral.ArtifactId): RIO[DataSource, Option[Long]] =
    Db.withDataSource:
      sql"select score from popular_daily where day = $day and group_id = ${g.toString} and artifact_id = ${a.toString}"
        .zQuery[Long].map(_.headOption)

  // recordSearchAppearance / recordListFilesClick fork-daemon their work, so
  // the call returns before Postgres has been touched. Poll until the score
  // matches what we expect, with a hard upper bound.
  private def awaitScore(g: MavenCentral.GroupId, a: MavenCentral.ArtifactId, expected: Long): RIO[DataSource, Option[Long]] =
    PopularMetrics.today.flatMap: day =>
      Live.live:
        score(day, g, a).delay(20.millis)
          .repeatUntil(_.contains(expected))
          .timeout(3.seconds)
          .map(_.flatten)

  private val rowCount: RIO[DataSource, Long] =
    Db.withDataSource(sql"select count(*) from popular_daily".zQuery[Long].map(_.head))

  def spec = suite("PopularMetrics")(

    test("recordListFilesClick gives a non-github artifact the full listfiles weight"):
      for
        _      <- popularMetrics.recordListFilesClick(groupId, jqueryId)
        score  <- awaitScore(groupId, jqueryId, 50)
      yield assertTrue(score.contains(50L)),

    test("recordListFilesClick discounts github-com artifacts to 0.3×"):
      for
        _      <- popularMetrics.recordListFilesClick(npmGroup, githubId)
        score  <- awaitScore(npmGroup, githubId, 15)
      yield assertTrue(score.contains(15L)),

    test("recordSearchAppearance writes one score per item with the right weights"):
      for
        _        <- popularMetrics.recordSearchAppearance(Seq(
                      (groupId, jqueryId),
                      (groupId, reactId),
                      (npmGroup, githubId),
                    ))
        jq       <- awaitScore(groupId, jqueryId, 10)
        rc       <- awaitScore(groupId, reactId, 10)
        gh       <- awaitScore(npmGroup, githubId, 3)
      yield assertTrue(
        jq.contains(10L),
        rc.contains(10L),
        gh.contains(3L),
      ),

    test("an artifact listed twice in one search is counted twice"):
      for
        _      <- popularMetrics.recordSearchAppearance(Seq((groupId, jqueryId), (groupId, jqueryId)))
        score  <- awaitScore(groupId, jqueryId, 20)
      yield assertTrue(score.contains(20L)),

    test("repeated recordings accumulate (listfiles + search on the same artifact)"):
      for
        _      <- popularMetrics.recordSearchAppearance(Seq((groupId, jqueryId)))
        _      <- popularMetrics.recordListFilesClick(groupId, jqueryId)
        score  <- awaitScore(groupId, jqueryId, 60)   // 10 + 50
      yield assertTrue(score.contains(60L)),

    test("recordSearchAppearance with an empty list writes nothing"):
      for
        _       <- popularMetrics.recordSearchAppearance(Seq.empty)
        _       <- Live.live(ZIO.sleep(200.millis))    // give a daemon a chance to misbehave
        count   <- rowCount
      yield assertTrue(count == 0L),

    test("Db.purgeExpired drops days past the retention window"):
      // purgeExpired compares against the database's date, not the TestClock.
      val today = LocalDate.now(java.time.ZoneOffset.UTC)
      def insert(day: LocalDate) =
        PostgresContainer.execute(sql"insert into popular_daily values ($day, 'org.webjars', 'jquery', 10)")
      for
        _      <- insert(today.minusDays(PopularMetrics.RetentionDays.toLong + 1))
        _      <- insert(today.minusDays(PopularMetrics.RetentionDays.toLong - 1))
        _      <- Db.purgeExpired
        days   <- Db.withDataSource(sql"select day from popular_daily".zQuery[LocalDate])
      yield assertTrue(days == Vector(today.minusDays(PopularMetrics.RetentionDays.toLong - 1))),

  ).provide(PostgresContainer.freshDatabase) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
