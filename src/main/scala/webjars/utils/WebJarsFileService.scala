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

  private val baseUrl = config.fileServiceUrl

  def getFileList(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, List[String]] =
    defer:
      val url = s"$baseUrl/listfiles/${gav.toPath.dropLeadingSlash}"
      val response = client.batched(Request.get(URL.decode(url).toOption.get)).run
      response.status match
        case Status.Ok =>
          val body = response.body.asString.run
          ZIO.fromEither(body.fromJson[List[String]].left.map(new Exception(_))).run
        case Status.NotFound =>
          val body = response.body.asString.run
          ZIO.fail(new FileNotFoundException(s"Could not get $url - $body")).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(s"Error fetching $url : $body")).run

  def getNumFiles(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Int] =
    defer:
      val url = s"$baseUrl/numfiles/${gav.toPath.dropLeadingSlash}"
      val response = client.batched(Request.get(URL.decode(url).toOption.get)).run
      response.status match
        case Status.Ok =>
          val body = response.body.asString.run
          ZIO.attempt(body.trim.toInt).run
        case Status.NotFound =>
          val body = response.body.asString.run
          ZIO.fail(new FileNotFoundException(s"Could not get $url - $body")).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(s"Error fetching $url : $body")).run

object WebJarsFileService:
  val live: ZLayer[Client & AppConfig, Nothing, WebJarsFileService] = ZLayer.derive[WebJarsFileServiceLive]
