package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import nl.vroste.rezilience.Timeout
import zio.*
import zio.cache.{Cache as ZCache, Lookup}
import zio.redis.Redis

/**
 * Bounded cache of `/listfiles` results, keyed by GAV.
 *
 * This replaces the general-purpose [[Cache]] for `/listfiles` because that
 * one never evicts keys: a client walking every published version kept each
 * file list in the heap for a day, which ran the dyno out of memory. This
 * cache holds at most [[capacity]] lists and evicts the least recently used.
 *
 * Concurrent misses for the same GAV share one upstream call. Failures are
 * not cached (zero TTL), so a transient file-service error doesn't stick.
 * Each upstream call is bounded by [[upstreamTimeout]]: the shared client
 * has no request timeout, and a hung file-service would otherwise pin
 * request fibers indefinitely.
 *
 * The numFiles backfill (see [[backfillNumFiles]]) runs only on a miss. It
 * used to run on every request, forking a daemon Redis read per hit.
 */
case class FileListCache(cache: ZCache[MavenCentral.GroupArtifactVersion, Throwable, List[String]]):
  def get(gav: MavenCentral.GroupArtifactVersion): Task[List[String]] =
    cache.get(gav)

object FileListCache:

  val capacity: Int = 1000
  val timeToLive: Duration = 1.day
  val upstreamTimeout: Duration = 15.seconds

  /**
   * The file-service has already produced the file list, so its size is the
   * version's numFiles. Only written when the cached version is missing it.
   * Forked so the response doesn't wait on Redis and a Redis blip can't fail
   * the request.
   */
  private def backfillNumFiles(gav: MavenCentral.GroupArtifactVersion, fileList: List[String]): URIO[Redis, Unit] =
    val ga = MavenCentral.GroupArtifact(gav.groupId, gav.artifactId)
    val version = gav.version.toString
    WebJarsCache.getArtifact(ga)
      .flatMap:
        case Some(meta) if meta.versions.exists(v => v.number == version && v.numFiles.isEmpty) =>
          WebJarsCache.updateVersion(ga, version, fileList.size)
        case _ =>
          ZIO.unit
      .tapErrorCause(c => ZIO.logWarningCause(s"listfiles numFiles backfill failed for $ga:$version", c))
      .ignore
      .forkDaemon
      .unit

  val live: ZLayer[WebJarsFileService & Redis, Nothing, FileListCache] =
    ZLayer.scoped:
      for
        fileService <- ZIO.service[WebJarsFileService]
        timeout     <- Timeout.make(upstreamTimeout)
        lookup       = Lookup[MavenCentral.GroupArtifactVersion, Redis, Throwable, List[String]]: gav =>
                         timeout(ZIO.scoped(fileService.getFileList(gav)))
                           .mapError(_.fold(Exception(s"Timed out listing files for $gav after $upstreamTimeout"), identity))
                           .tap(backfillNumFiles(gav, _))
        cache       <- ZCache.makeWith(capacity, lookup):
                         case Exit.Success(_) => timeToLive
                         case Exit.Failure(_) => Duration.Zero
      yield FileListCache(cache)
