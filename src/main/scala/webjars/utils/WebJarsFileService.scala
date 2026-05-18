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
          ZIO.fail(new FileNotFoundException(s"Could not get ${url.encode} - $body")).run
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
          ZIO.fail(new FileNotFoundException(s"Could not get ${url.encode} - $body")).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(s"Error fetching ${url.encode} : $body")).run

object WebJarsFileService:
  val live: ZLayer[Client & AppConfig, Nothing, WebJarsFileService] = ZLayer.derive[WebJarsFileServiceLive]
