package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

import java.io.FileNotFoundException

trait WebJarsFileService:
  def getFileList(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, List[String]]
  def getNumFiles(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Int]

case class WebJarsFileServiceLive(client: Client, config: AppConfig) extends WebJarsFileService:

  // Parse once at construction time so a malformed config fails fast rather
  // than at request time.
  private val baseUrl: URL = URL.decode(config.fileServiceUrl).toOption.getOrElse(
    throw new IllegalStateException(s"Invalid webjars.file-service.url: ${config.fileServiceUrl}")
  )

  // Build URLs with typed Path append so versions containing special chars
  // (e.g. bootswatch 3.3.4+1) survive without manual escaping.
  private def serviceUrl(endpoint: String, gav: MavenCentral.GroupArtifactVersion): URL =
    baseUrl.addPath(Path.root / endpoint).addPath(gav.toPath)

  def getFileList(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, List[String]] =
    defer:
      val url = serviceUrl("listfiles", gav)
      val response = client.batched(Request.get(url)).run
      response.status match
        case Status.Ok =>
          val body = response.body.asString.run
          ZIO.fromEither(body.fromJson[List[String]].left.map(new Exception(_))).run
        case Status.NotFound =>
          val body = response.body.asString.run
          ZIO.fail(new WebJarsFileService.WebJarNotFoundException(s"Could not get ${url.encode} - $body")).run
        case Status.UnprocessableEntity =>
          // 422: jar exists on Maven Central but its contents can't be
          // extracted (corrupt zip, encoding error, etc.). Permanent —
          // same tombstone treatment as a missing jar; see
          // `UnusableWebJarException` for the marker hierarchy.
          val body = response.body.asString.run
          ZIO.fail(new WebJarsFileService.WebJarUnprocessableException(s"Could not process ${url.encode} (corrupt jar) - $body")).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(s"Error fetching ${url.encode} : $body")).run

  def getNumFiles(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Int] =
    defer:
      val url = serviceUrl("numfiles", gav)
      val response = client.batched(Request.get(url)).run
      response.status match
        case Status.Ok =>
          val body = response.body.asString.run
          ZIO.attempt(body.trim.toInt).run
        case Status.NotFound =>
          val body = response.body.asString.run
          ZIO.fail(new WebJarsFileService.WebJarNotFoundException(s"Could not get ${url.encode} - $body")).run
        case Status.UnprocessableEntity =>
          val body = response.body.asString.run
          ZIO.fail(new WebJarsFileService.WebJarUnprocessableException(s"Could not process ${url.encode} (corrupt jar) - $body")).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(s"Error fetching ${url.encode} : $body")).run

object WebJarsFileService:

  /** Marker for "this version of this webjar is permanently unusable",
   *  emitted when the file-service responds with a status that
   *  indicates a problem inherent to the published artifact (jar
   *  missing or corrupt) rather than a transient transport/server
   *  issue. Consumers — `MavenCentralWebJars.refreshArtifact` /
   *  `refreshMissingNumFiles` — pattern-match on this trait to
   *  add a version-level tombstone and remove the version from
   *  `WebJarMeta.versions` so it stops appearing in /, /popular,
   *  /search, /all, /list/:groupId, etc. */
  sealed trait UnusableWebJarException extends Throwable

  /** 404 from the file-service: the jar isn't published on Maven
   *  Central (broken publish — only the .pom in the version's
   *  directory). Extends [[FileNotFoundException]] so existing
   *  consumers (e.g. `AppRoutes.handleListFiles`'s catchAll, which
   *  returns 404 to the user) keep working without modification. */
  class WebJarNotFoundException(message: String)
    extends FileNotFoundException(message) with UnusableWebJarException

  /** 422 from the file-service: the jar IS published but its contents
   *  can't be processed (corrupt zip, encoding error, etc.). Same
   *  tombstone treatment as [[WebJarNotFoundException]] — the version
   *  can never produce a useful file list. NOT a `FileNotFoundException`
   *  because semantically the file does exist; consumers that want
   *  the unified "permanently unusable" semantics should match on
   *  [[UnusableWebJarException]]. */
  class WebJarUnprocessableException(message: String)
    extends Exception(message) with UnusableWebJarException

  val live: ZLayer[Client & AppConfig, Nothing, WebJarsFileService] = ZLayer.derive[WebJarsFileServiceLive]
