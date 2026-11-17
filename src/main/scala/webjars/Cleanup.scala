package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.MavenCentralRepo
import webjars.config.AppConfig
import webjars.utils.*
import zio.*
import zio.http.Client
import javax.sql.DataSource

/**
 * One-off cleanup of parent POMs (packaging=pom) that accumulated in
 * `WebJarsCache` before the going-forward fix landed: multi-module parents
 * like `org.webjars:when-parent` that have no jar contents and shouldn't
 * be in cache.
 *
 * Going forward, `MavenCentralWebJars.refreshArtifact` skips and tombstones
 * pom-packaged artifacts (and the `webjar` table rejects empty artifactIds,
 * the other legacy corruption).
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
    val program: ZIO[Client & DataSource & MavenCentralRepo & MavenCentralWebJars & AllDeployables, Throwable, Unit] =
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
      Db.fromEnv("cleanup", 3),
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
