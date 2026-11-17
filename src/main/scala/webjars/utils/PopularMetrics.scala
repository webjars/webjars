package webjars.utils

import com.augustnagro.magnum.*
import com.augustnagro.magnum.pg.PgCodec.given
import com.augustnagro.magnum.ziomagnum.*
import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.Db.given
import zio.*

import java.time.{LocalDate, ZoneOffset}
import javax.sql.DataSource

// Background-only popularity tracking. Every recorded event fans out to a
// forked daemon fiber, so callers (HTTP handlers) never wait on Postgres.
// Errors are logged at WARN and dropped — metrics are best-effort.
//
// Data model: table `popular_daily`, one row per (UTC day, groupId,
// artifactId) holding the day's cumulative weight. Rows older than
// `RetentionDays` (30-day window + 2-day buffer) are purged by
// `Db.purgeExpired`.
//
// Weights are scaled by 10 so scores stay integers. github-com-* artifacts
// get a 3× discount per the spec.
trait PopularMetrics:
  def recordSearchAppearance(items: Seq[(MavenCentral.GroupId, MavenCentral.ArtifactId)]): URIO[DataSource, Unit]
  def recordListFilesClick(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): URIO[DataSource, Unit]

case class PopularMetricsLive() extends PopularMetrics:

  // Scaled weights — see file header for why these aren't Doubles.
  private val SearchWeight    = 10L    // 1.0 × 10
  private val ListFilesWeight = 50L    // 5.0 × 10
  private val GitHubComMultiplierNumer = 3L
  private val GitHubComMultiplierDenom = 10L

  private def adjustedWeight(base: Long, artifactId: MavenCentral.ArtifactId): Long =
    if artifactId.toString.toLowerCase.contains("github-com") then
      (base * GitHubComMultiplierNumer) / GitHubComMultiplierDenom
    else
      base

  // One statement per event, however many artifacts it covers. Duplicates are
  // summed first (an upsert can't touch a row twice), and rows are written in
  // key order so concurrent batches lock shared rows in the same order.
  private def writeBatch(label: String, items: Seq[(MavenCentral.GroupId, MavenCentral.ArtifactId, Long)]): URIO[DataSource, Unit] =
    if items.isEmpty then ZIO.unit
    else
      val summed = items.groupMapReduce((g, a, _) => (g.toString, a.toString))(_._3)(_ + _).toSeq.sortBy(_._1)
      val groupIds: Seq[String]    = summed.map(_._1._1)
      val artifactIds: Seq[String] = summed.map(_._1._2)
      val weights: Seq[Long]       = summed.map(_._2)
      PopularMetrics.today.flatMap: day =>
        Db.withDataSource:
          sql"""insert into popular_daily (day, group_id, artifact_id, score)
                select $day, g, a, w from unnest($groupIds::text[], $artifactIds::text[], $weights::bigint[]) as t(g, a, w)
                order by g, a
                on conflict (day, group_id, artifact_id) do update set score = popular_daily.score + excluded.score""".zUpdate
      .tapErrorCause(c => ZIO.logWarningCause(s"PopularMetrics.$label failed", c))
      .ignore
      .forkDaemon
      .unit

  def recordSearchAppearance(items: Seq[(MavenCentral.GroupId, MavenCentral.ArtifactId)]): URIO[DataSource, Unit] =
    writeBatch("recordSearchAppearance", items.map((g, a) => (g, a, adjustedWeight(SearchWeight, a))))

  def recordListFilesClick(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): URIO[DataSource, Unit] =
    writeBatch("recordListFilesClick", Seq((groupId, artifactId, adjustedWeight(ListFilesWeight, artifactId))))

object PopularMetrics:
  val RetentionDays: Int = 32

  /** The current UTC day, from the ZIO `Clock`. */
  val today: UIO[LocalDate] =
    Clock.currentDateTime.map(_.atZoneSameInstant(ZoneOffset.UTC).toLocalDate)

  val live: ULayer[PopularMetrics] = ZLayer.succeed(PopularMetricsLive())
