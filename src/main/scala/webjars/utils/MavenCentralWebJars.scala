package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.MavenCentralRepo
import webjars.config.AppConfig
import webjars.models.{WebJar, WebJarVersion}
import zio.*
import zio.direct.*
import zio.http.Client
import zio.redis.Redis

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import scala.xml.Elem

trait MavenCentralWebJars:
  def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentralRepo, Throwable, Elem]
  private[utils] def refreshGroup(groupId: MavenCentral.GroupId, updateNumFiles: Boolean = true): ZIO[Client & Redis & MavenCentralRepo, Throwable, Set[MavenCentral.ArtifactId]]
  def refreshAll(groupIds: Set[MavenCentral.GroupId]): ZIO[Client & Redis & MavenCentralRepo, Nothing, Unit]
  def startRefreshLoop(): ZIO[Client & Redis & MavenCentralRepo, Nothing, Fiber.Runtime[Nothing, Unit]]
  def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): ZIO[Redis, Throwable, Seq[WebJar]]
  /** On-demand refresh of a single artifact's version list from Maven
   *  Central. Used by the UI "show me current versions" path on Classic
   *  WebJars that can't be deployed through this app. Returns the resulting
   *  versions list from the cache (post-refresh). */
  def refreshArtifactNow(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Redis & MavenCentralRepo, Throwable, List[WebJarVersion]]

case class MavenCentralWebJarsLive(config: AppConfig, webJarsFileService: WebJarsFileService, allDeployables: AllDeployables, searchIndex: SearchIndex) extends MavenCentralWebJars:

  def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentralRepo, Throwable, Elem] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version).mapError {
      case _: MavenCentral.NotFoundError => FileNotFoundException("pom not found")
      case t: Throwable => t
    }

  // Mirror fallback + per-mirror circuit breakers in `MavenCentralRepo`
  // replace the per-call `retryOnServerError` we used in 0.8.x — transient
  // 5xx / 429 / 403 are now handled inside the repo layer.
  private def fetchWebJarNameAndUrl(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentralRepo, Nothing, (String, String)] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version)
      .flatMap: xml =>
        val artifactId = (xml \ "artifactId").text
        val rawName = (xml \ "name").text
        val name = if rawName.contains("${") || rawName.isEmpty then artifactId else rawName

        val rawUrl = (xml \ "scm" \ "url").text

        val url = if rawUrl.contains("${") then
          ZIO.fail(new RuntimeException("Unable to determine source url"))
        else
          if rawUrl != "" then
            ZIO.succeed(rawUrl)
          else
            val parentGroupId = (xml \ "parent" \ "groupId").text
            val parentArtifactId = (xml \ "parent" \ "artifactId").text
            val parentVersion = (xml \ "parent" \ "version").text
            MavenCentral.pom(MavenCentral.GroupId(parentGroupId), MavenCentral.ArtifactId(parentArtifactId), MavenCentral.Version(parentVersion))
              .map { parentXml => (parentXml \ "scm" \ "url").text }

        url.map(name -> _)
      .catchAll: _ =>
        ZIO.succeed:
          gav.artifactId.toString -> s"https://github.com/webjars/${gav.artifactId}"

  private def fetchVersions(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[MavenCentralRepo, Throwable, MavenCentral.WithCacheInfo[Seq[MavenCentral.Version]]] =
    MavenCentral.searchVersions(groupId, artifactId)
      .mapError {
        case _: MavenCentral.GroupIdOrArtifactIdNotFoundError => Throwable("groupId or artifactId not found")
        case t: Throwable => t
      }

  private def fetchArtifactIds(groupId: MavenCentral.GroupId): ZIO[MavenCentralRepo, Throwable, MavenCentral.WithCacheInfo[Seq[MavenCentral.ArtifactId]]] =
    MavenCentral.searchArtifacts(groupId).mapBoth({
      case _: MavenCentral.GroupIdNotFoundError => Throwable("groupId not found")
      case t: Throwable => t
    }, {
      artifacts =>
        artifacts.copy(value = artifacts.value.filterNot { artifactId =>
          artifactId.toString.startsWith("webjars-") ||
            artifactId.toString == "bower" ||
            artifactId.toString == "bowergithub" ||
            artifactId.toString == "2.11.2"
        })
    })

  private def refreshArtifact(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, versions: Seq[WebJarVersion], updateNumFiles: Boolean): ZIO[Client & Redis & MavenCentralRepo & Scope, Throwable, Unit] =
    ZIO.logInfo(s"Refreshing artifact: $groupId:$artifactId") *> {
      val refresh = defer:
        val versionsResult = fetchVersions(groupId, artifactId).run
        val latestVersion = versionsResult.value.head
        val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, latestVersion)
        val (name, url) = fetchWebJarNameAndUrl(gav).run
        WebJarsCache.setArtifactDetails(
          gav.noVersion,
          WebJarsCache.WebJarMeta(name, url, versionsResult.value.map(v => WebJarVersion(v.toString, None)).toList, versionsResult.maybeLastModified.getOrElse(ZonedDateTime.now()))
        ).run

        val resolvedVersions = versions.flatMap: webJarVersion =>
          webJarVersion.numFiles.map(_ => webJarVersion.number)

        val versionsToRefresh = versionsResult.value.diff(resolvedVersions)

        ZIO.when(updateNumFiles):
          ZIO.foreachPar(versionsToRefresh) { version =>
            webJarsFileService.getNumFiles(gav.copy(version = version)).flatMap { numFiles =>
              WebJarsCache.updateVersion(gav.noVersion, version.toString, numFiles)
            }
          }.withParallelism(5).unit
          .forkDaemon
        .unit.run

      refresh.tapError(error => ZIO.logError(s"Error refreshing artifact $groupId:$artifactId: ${error.getMessage}"))
    }

  // Background refresh: only picks up artifacts that exist on Maven Central
  // but aren't yet in our cache. We deliberately do NOT iterate
  // `cachedArtifacts` calling `isModifiedSince` per entry — that pattern was
  // the dominant source of 429-storms (one HEAD per artifact × every cycle).
  // Detecting version changes for already-cached artifacts is a separate
  // problem we'll solve later (likely demand-driven or via a feed).
  private[utils] def refreshGroup(groupId: MavenCentral.GroupId, updateNumFiles: Boolean = true): ZIO[Client & Redis & MavenCentralRepo, Throwable, Set[MavenCentral.ArtifactId]] =
    ZIO.logInfo(s"Refreshing groupId: $groupId with updateNumFiles: $updateNumFiles") *>
    ZIO.scoped:
      defer:
        val artifactsResult  = fetchArtifactIds(groupId).run
        val cachedArtifacts  = WebJarsCache.getArtifacts(groupId).run
        val tombstoned       = WebJarsCache.getTombstones(groupId).run
        val missingArtifacts = artifactsResult.value.toSet -- cachedArtifacts.keySet -- tombstoned
        ZIO.logInfo(
          s"Refresh plan for $groupId: ${artifactsResult.value.size} on Maven Central, " +
          s"${cachedArtifacts.size} in cache, ${tombstoned.size} tombstoned, " +
          s"${missingArtifacts.size} to fetch"
        ).run
        // On success → return the id (counted as refreshed). On failure →
        // tombstone the id so the next cycle skips it, and return None.
        val refreshed = ZIO.foreachPar(missingArtifacts.toSeq): artifactId =>
          refreshArtifact(groupId, artifactId, Seq.empty, updateNumFiles)
            .as(Option(artifactId))
            .catchAll: _ =>
              WebJarsCache.addTombstone(groupId, artifactId).ignoreLogged.as(None)
        .withParallelism(5).map(_.flatten.toSet).run
        ZIO.logInfo(s"Refresh done for $groupId: ${refreshed.size} new artifacts written, ${missingArtifacts.size - refreshed.size} tombstoned").run
        refreshed

  // Pick up newly-deployed versions for already-cached artifacts. Reads
  // the pending-deploys queue (written by `DeployWebJar` on successful
  // publish), refreshes each affected artifact's metadata from MC, and
  // removes pending entries whose version has landed in cache. Entries
  // older than 48h are dropped — assumed-abandoned deploys never propagate.
  private[utils] def refreshPendingDeploys(): ZIO[Client & Redis & MavenCentralRepo, Throwable, Unit] =
    ZIO.scoped:
      defer:
        val staleCount = WebJarsCache.cullStalePendingDeploys(48.hours).run
        ZIO.when(staleCount > 0):
          ZIO.logInfo(s"Pending deploys: culled $staleCount stale entries (>48h old)")
        .run

        val pending = WebJarsCache.getPendingDeploys.run
        if pending.isEmpty then ()
        else
          ZIO.logInfo(s"Pending deploys: processing ${pending.size} entries across ${pending.groupBy(p => (p.groupId, p.artifactId)).size} artifacts").run
          val byArtifact = pending.groupBy(p => (p.groupId, p.artifactId))
          ZIO.foreachParDiscard(byArtifact.toSeq) { case ((gid, aid), entries) =>
            val ga = MavenCentral.GroupArtifact(gid, aid)
            val refreshAndCheck = defer:
              val existing         = WebJarsCache.getArtifact(ga).run
              val existingVersions = existing.map(_.versions).getOrElse(List.empty)
              refreshArtifact(gid, aid, existingVersions, updateNumFiles = true).run
              val updated          = WebJarsCache.getArtifact(ga).run
              val updatedNumbers   = updated.map(_.versions.map(_.number).toSet).getOrElse(Set.empty)
              ZIO.foreachDiscard(entries): p =>
                if updatedNumbers.contains(p.version.toString) then
                  WebJarsCache.removePendingDeploy(p) *>
                    ZIO.logInfo(s"Pending deploy resolved: $gid:$aid:${p.version}")
                else
                  ZIO.logInfo(s"Pending deploy still waiting (not yet on MC): $gid:$aid:${p.version}")
              .run
            refreshAndCheck.tapErrorCause(c => ZIO.logErrorCause(s"refreshPendingDeploys failed for $gid:$aid", c)).ignore
          }.withParallelism(3).run

  def refreshAll(groupIds: Set[MavenCentral.GroupId]): ZIO[Client & Redis & MavenCentralRepo, Nothing, Unit] =
    ZIO.foreachDiscard(groupIds)(groupId => refreshGroup(groupId).ignoreLogged) *>
      refreshPendingDeploys().ignoreLogged *>
      searchIndex.rebuild.forkDaemon.unit

  def startRefreshLoop(): ZIO[Client & Redis & MavenCentralRepo, Nothing, Fiber.Runtime[Nothing, Unit]] =
    val once = refreshAll(allDeployables.groupIds())
    val effect = config.mavenCentralRefreshInterval.fold(once)(interval => once.repeat(Schedule.spaced(interval)).unit)
    effect
      .tapErrorCause(c => ZIO.logErrorCause("Maven Central refresh loop crashed", c))
      .forkDaemon

  extension (m: Map[MavenCentral.ArtifactId, WebJarsCache.WebJarMeta])
    private def toWebJars(groupId: MavenCentral.GroupId): Seq[WebJar] =
      m.map: (artifactId, meta) =>
        WebJar(groupId.toString, artifactId.toString, meta.name, meta.sourceUrl, meta.versions.toSeq)
      .toSeq

  def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): ZIO[Redis, Throwable, Seq[WebJar]] =
    WebJarsCache.getArtifacts(groupId, query = query).map(_.toWebJars(groupId))

  def refreshArtifactNow(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Redis & MavenCentralRepo, Throwable, List[WebJarVersion]] =
    val ga = MavenCentral.GroupArtifact(groupId, artifactId)
    ZIO.scoped:
      defer:
        val existing         = WebJarsCache.getArtifact(ga).run
        val existingVersions = existing.map(_.versions).getOrElse(List.empty)
        refreshArtifact(groupId, artifactId, existingVersions, updateNumFiles = true).run
        val updated          = WebJarsCache.getArtifact(ga).run
        updated.map(_.versions).getOrElse(List.empty)

object MavenCentralWebJars:
  val live: ZLayer[AppConfig & WebJarsFileService & AllDeployables & SearchIndex, Nothing, MavenCentralWebJars] = ZLayer.derive[MavenCentralWebJarsLive]
