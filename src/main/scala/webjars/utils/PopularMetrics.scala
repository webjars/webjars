package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.redis.*

import java.time.ZoneOffset

// Background-only popularity tracking. Every recorded event fans out to a
// forked daemon fiber, so callers (HTTP handlers) never wait on Redis. Errors
// are logged at WARN and dropped — metrics are best-effort.
//
// Data model:
//   ZSET key   = "popular:<YYYY-MM-DD>" (UTC date)
//   ZSET member = "<groupId>:<artifactId>"  (version dropped — see TODO-SPEC.md)
//   ZSET score  = cumulative weight for the day
//   TTL         = 32 days (30-day rolling window + 2-day buffer)
//
// Weights are scaled by 10 because zio-redis 1.2.1's `zIncrBy` takes Long, not
// Double — Redis itself stores scores as floats so the relative ordering is
// preserved. github-com-* artifacts get a 3× discount per the spec.
trait PopularMetrics:
  def recordSearchAppearance(items: Seq[(MavenCentral.GroupId, MavenCentral.ArtifactId)]): UIO[Unit]
  def recordListFilesClick(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): UIO[Unit]

case class PopularMetricsLive(valkey: Valkey) extends PopularMetrics:

  // Scaled weights — see file header for why these aren't Doubles.
  private val SearchWeight    = 10L    // 1.0 × 10
  private val ListFilesWeight = 50L    // 5.0 × 10
  private val GitHubComMultiplierNumer = 3L
  private val GitHubComMultiplierDenom = 10L
  private val WindowDays      = 32

  private def adjustedWeight(base: Long, artifactId: MavenCentral.ArtifactId): Long =
    if artifactId.toString.toLowerCase.contains("github-com") then
      (base * GitHubComMultiplierNumer) / GitHubComMultiplierDenom
    else
      base

  private val todayKey: UIO[String] =
    Clock.currentDateTime.map: now =>
      val date = now.atZoneSameInstant(ZoneOffset.UTC).toLocalDate.toString
      s"popular:$date"

  private def writeBatch(label: String, members: Seq[(String, Long)]): UIO[Unit] =
    if members.isEmpty then ZIO.unit
    else
      todayKey.flatMap: key =>
        val write =
          ZIO.serviceWithZIO[Redis]: redis =>
            ZIO.foreachDiscard(members):
              case (member, weight) => redis.zIncrBy(key, weight, member).unit
            *> redis.expire(key, WindowDays.days).unit
        write
          .tapErrorCause(c => ZIO.logWarningCause(s"PopularMetrics.$label failed", c))
          .ignore
          .provide(valkey.layer)
          .forkDaemon
          .unit

  def recordSearchAppearance(items: Seq[(MavenCentral.GroupId, MavenCentral.ArtifactId)]): UIO[Unit] =
    val members = items.map: (groupId, artifactId) =>
      s"${groupId.toString}:${artifactId.toString}" -> adjustedWeight(SearchWeight, artifactId)
    writeBatch("recordSearchAppearance", members)

  def recordListFilesClick(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): UIO[Unit] =
    val member = s"${groupId.toString}:${artifactId.toString}"
    writeBatch("recordListFilesClick", Seq(member -> adjustedWeight(ListFilesWeight, artifactId)))

object PopularMetrics:
  val live: ZLayer[Valkey, Nothing, PopularMetrics] = ZLayer.derive[PopularMetricsLive]
