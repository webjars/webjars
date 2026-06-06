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
  /** Backfill pass: walks every cached artifact in `groupId`, finds
   *  versions whose `numFiles` is still `None`, and re-attempts the
   *  file-service call. Designed to repair the historical leak where
   *  `refreshArtifact` would `forkDaemon` the per-version numFiles work
   *  and lose it on restart, plus to recover from transient file-service
   *  failures during initial refresh.
   *
   *  Per-version failures are isolated and logged — one slow or broken
   *  version cannot poison sibling versions or other artifacts. Cheap in
   *  steady state because the file-service has its own caching: once a
   *  `numFiles` is filled, this method skips it. */
  def refreshMissingNumFiles(groupId: MavenCentral.GroupId): ZIO[Client & Redis, Throwable, Unit]
  def refreshAll(groupIds: Set[MavenCentral.GroupId]): ZIO[Client & Redis & MavenCentralRepo, Nothing, Unit]
  def startRefreshLoop(): ZIO[Client & Redis & MavenCentralRepo, Nothing, Fiber.Runtime[Nothing, Unit]]
  def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): ZIO[Redis, Throwable, Seq[WebJar]]
  /** On-demand refresh of a single artifact's version list from Maven
   *  Central. Used by the UI "show me current versions" path on Classic
   *  WebJars that can't be deployed through this app. Returns the resulting
   *  versions list from the cache (post-refresh). */
  def refreshArtifactNow(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Redis & MavenCentralRepo, Throwable, List[WebJarVersion]]

case class MavenCentralWebJarsLive(config: AppConfig, webJarsFileService: WebJarsFileService, allDeployables: AllDeployables, searchIndex: SearchIndex, popularRanking: PopularRanking) extends MavenCentralWebJars:

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

        // numFiles work runs in-band (NOT `forkDaemon`) so it survives
        // process restarts. The historical bug here: forking off the
        // numFiles fetch meant a restart mid-cycle left versions stuck
        // at `None` forever (refreshGroup only refreshes *missing*
        // artifacts on subsequent cycles, so it never came back to
        // them). The new `refreshMissingNumFiles` pass also catches
        // anything that slipped through, but doing the work in-band on
        // first write closes the common case.
        //
        // Per-version failures are logged-and-ignored so a single
        // 5xx/timeout from the file-service can't fail the whole
        // artifact refresh — which would otherwise tombstone the
        // artifact in the surrounding `refreshGroup` catch and cost us
        // its metadata too.
        //
        // We collect all numFiles in parallel then write the artifact
        // ONCE — `WebJarsCache.updateVersion` is a non-atomic
        // read-modify-write, so calling it concurrently for the same
        // artifact races and clobbers writes. Single setArtifactDetails
        // is atomic per artifact.
        ZIO.when(updateNumFiles && versionsToRefresh.nonEmpty):
          val fetched = ZIO.foreachPar(versionsToRefresh) { version =>
            val versionGav = gav.copy(version = version)
            webJarsFileService.getNumFiles(versionGav)
              .map(nf => Some(version.toString -> nf))
              .tapError(error => ZIO.logWarning(s"numFiles fetch failed for $versionGav: ${error.getMessage}"))
              .catchAll(_ => ZIO.none)
          }.withParallelism(5).map(_.flatten.toMap)

          fetched.flatMap { results =>
            if results.isEmpty then ZIO.unit
            else
              WebJarsCache.getArtifact(gav.noVersion).flatMap {
                case None       => ZIO.unit  // race: artifact disappeared
                case Some(meta) =>
                  val patched = meta.versions.map { v =>
                    results.get(v.number).fold(v)(nf => v.copy(numFiles = Some(nf)))
                  }
                  WebJarsCache.setArtifactDetails(gav.noVersion, meta.copy(versions = patched))
              }
          }
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
      // Backfill numFiles that were never filled — e.g. fetched when the
      // file-service was down, lost to a process restart before the old
      // `forkDaemon` completed, or originally written by a prior version
      // of this code. Runs every refresh cycle. No-op for fully-populated
      // groups; bounded by `versionsToBackfill × file-service-call-time
      // / 5`.
      ZIO.foreachDiscard(groupIds)(groupId => refreshMissingNumFiles(groupId).ignoreLogged) *>
      // Aggregate-cache rebuilds: both `searchIndex` (powers /search,
      // /all, /list/:groupId) and `popularRanking` (powers / and
      // /popular) hold in-memory snapshots derived from WebJarsCache. We
      // rebuild both on every refresh cycle so updates from the steps
      // above — most importantly the numFiles backfill — propagate to
      // the user-visible pages. Without the popularRanking rebuild the
      // home page stays frozen at the boot-time snapshot until the
      // dyno cycles (~24h on Heroku).
      searchIndex.rebuild.forkDaemon *>
      popularRanking.rebuild.forkDaemon.unit

  def refreshMissingNumFiles(groupId: MavenCentral.GroupId): ZIO[Client & Redis, Throwable, Unit] =
    ZIO.logInfo(s"Backfilling missing numFiles for groupId: $groupId") *>
    ZIO.scoped:
      defer:
        val cachedArtifacts = WebJarsCache.getArtifacts(groupId).run
        val totalMissing    = cachedArtifacts.values.map(_.versions.count(_.numFiles.isEmpty)).sum

        ZIO.logInfo(s"Backfill plan for $groupId: $totalMissing version(s) need numFiles across ${cachedArtifacts.size} artifact(s)").run

        // Per-artifact pass. We deliberately do a single atomic
        // setArtifactDetails per artifact rather than calling
        // WebJarsCache.updateVersion in parallel: the latter is a
        // read-modify-write that's NOT safe under concurrency, and
        // historical bug here clobbered sibling updates. Within an
        // artifact: fetch all missing versions in parallel (cheap), then
        // patch the version list and write once.
        val filledRef = ZIO.foreachPar(cachedArtifacts.toSeq) { (artifactId, _) =>
          val ga = MavenCentral.GroupArtifact(groupId, artifactId)
          val versionGav = (versionStr: String) =>
            MavenCentral.GroupArtifactVersion(groupId, artifactId, MavenCentral.Version(versionStr))

          // Re-read the artifact under the per-artifact lock window we
          // implicitly hold (we're the only writer to numFiles in this
          // group). Cheap and keeps the read/write window small.
          val perArtifact = defer:
            val current = WebJarsCache.getArtifact(ga).run
            current match
              case None       => 0
              case Some(meta) =>
                val missingVersions = meta.versions.filter(_.numFiles.isEmpty).map(_.number)
                if missingVersions.isEmpty then 0
                else
                  // Fetch each missing version's numFiles in parallel;
                  // collect successes into a Map. Per-version failures
                  // are isolated and logged.
                  val fetched = ZIO.foreachPar(missingVersions) { versionStr =>
                    webJarsFileService.getNumFiles(versionGav(versionStr))
                      .map(nf => Some(versionStr -> nf))
                      .tapError(error => ZIO.logWarning(s"Backfill of numFiles failed for ${versionGav(versionStr)}: ${error.getMessage}"))
                      .catchAll(_ => ZIO.none)
                  }.withParallelism(3).map(_.flatten.toMap).run

                  if fetched.isEmpty then 0
                  else
                    val patched = meta.versions.map { v =>
                      fetched.get(v.number).fold(v)(nf => v.copy(numFiles = Some(nf)))
                    }
                    WebJarsCache.setArtifactDetails(ga, meta.copy(versions = patched)).run
                    fetched.size

          perArtifact
            .tapError(error => ZIO.logWarning(s"Backfill failed for $ga: ${error.getMessage}"))
            .catchAll(_ => ZIO.succeed(0))
        }.withParallelism(5).map(_.sum)

        val filled = filledRef.run

        ZIO.when(totalMissing > 0):
          ZIO.logInfo(s"Backfill done for $groupId: filled $filled / $totalMissing version(s)")
        .unit.run

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
  val live: ZLayer[AppConfig & WebJarsFileService & AllDeployables & SearchIndex & PopularRanking, Nothing, MavenCentralWebJars] = ZLayer.derive[MavenCentralWebJarsLive]
