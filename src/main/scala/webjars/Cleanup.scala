package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.MavenCentralRepo
import webjars.config.AppConfig
import webjars.utils.*
import zio.*
import zio.http.Client
import zio.redis.Redis

/**
 * One-off cleanup of stale entries that accumulated in `WebJarsCache`
 * before the going-forward fixes landed:
 *
 *   1. Empty-artifactId entries from pre-0.9.x zio-mavencentral parsing
 *      (a regex bug that let `<a href="/">` directory-listing links
 *      leak through as empty-string artifactIds).
 *
 *   2. Parent POMs (packaging=pom) — multi-module parents like
 *      `org.webjars:when-parent` that have no jar contents and shouldn't
 *      be in cache.
 *
 * Going forward, both categories are blocked at the cache write boundary:
 *   - `WebJarsCache.setArtifactDetails` rejects empty artifactIds.
 *   - `MavenCentralWebJars.refreshArtifact` skips and tombstones
 *     pom-packaged artifacts.
 *
 * So this is a one-time migration. Run after deploying the going-forward
 * fixes, then forget about it:
 *
 * {{{
 * heroku run -a webjars sbt 'runMain webjars.Cleanup'
 * }}}
 *
 * It iterates every configured groupId (whatever `AllDeployables.groupIds()`
 * returns at the time of invocation) and walks each cached artifact's POM
 * to detect packaging. Idempotent — safe to re-run if it fails partway
 * through. Tombstones each removed artifact so the next refresh cycle
 * doesn't re-discover it (saves a redundant POM fetch).
 */
object Cleanup extends ZIOAppDefault:

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = Logging.bootstrap

  def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
    val program: ZIO[Client & Redis & MavenCentralRepo & MavenCentralWebJars & AllDeployables, Throwable, Unit] =
      for
        allDeployables <- ZIO.service[AllDeployables]
        mc             <- ZIO.service[MavenCentralWebJars]
        groups         = allDeployables.groupIds()
        _              <- ZIO.logInfo(s"Cleanup starting for ${groups.size} group(s): ${groups.map(_.toString).toList.sorted.mkString(", ")}")
        // Sequential so the per-group log lines stay readable. Within a
        // group, `cleanupCachedArtifacts` parallelizes POM fetches.
        totals         <- ZIO.foreach(groups.toSeq) { groupId =>
                            mc.cleanupCachedArtifacts(groupId).map(groupId.toString -> _)
                          }
        grand          = totals.map(_._2).sum
        _              <- ZIO.foreachDiscard(totals) { (gid, n) =>
                            ZIO.logInfo(s"Cleanup result for $gid: $n entr(ies) removed")
                          }
        _              <- ZIO.logInfo(s"Cleanup done: $grand entr(ies) removed across ${groups.size} group(s)")
      yield ()

    // The layer set mirrors `Deploy.scala`'s — same dependency graph,
    // minus the Sonatype / GPG bits (cleanup doesn't deploy). We use
    // `MavenCentralWebJars.live` so we get the real `cleanupCachedArtifacts`
    // implementation.
    program.provide(
      AppConfig.live,
      Client.default,
      Cache.live,
      Valkey.live,
      Git.live,
      GitHub.live,
      SemVer.live,
      Maven.live,
      WebJarsFileService.live,
      NPM.live,
      Classic.live,
      AllDeployables.live,
      MavenCentralRepo.live,
      MavenCentralWebJars.live,
      SearchIndex.live,
      PopularRanking.live,
    )
