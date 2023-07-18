package utils

import zio._
import zio.http.{Client, Status}
import zio.direct._
import zio.json.JsonDecoder

import java.io.FileNotFoundException
import javax.inject.Singleton
import scala.concurrent.Future

@Singleton
class WebJarsFileService {
  private val runtime: Runtime[Any] = Runtime.default
  private val client: ZLayer[Any, Throwable, Client] = Client.default

  private def zioToFuture[A](zio: ZIO[Client, Throwable, A]): Future[A] = {
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.runToFuture(zio.provide(client))
    }
  }

  def getFileList(groupId: String, artifactId: String, version: String): Future[List[String]] =
    zioToFuture(WebJarsFileService.getFileList(groupId, artifactId, version))

  def getNumFiles(groupId: String, artifactId: String, version: String): Future[Int] =
    zioToFuture(WebJarsFileService.getNumFiles(groupId, artifactId, version))
}

object WebJarsFileService {

  def getFileList(groupId: String, artifactId: String, version: String): ZIO[Client, Throwable, List[String]] = {
    val url = s"https://webjars-file-service.herokuapp.com/listfiles/$groupId/$artifactId/$version"

    defer {
      val response = Client.request(url).run
      response.status match {
        case Status.Ok =>
          val filesStream = response.body.asStream
          JsonDecoder[List[String]].decodeJsonStreamInput(filesStream).run
        case Status.NotFound =>
          throw new FileNotFoundException(s"Could not get $url - ${response.body}")
        case _ =>
          throw new Exception(s"Error fetching $url : ${response.body}")
      }
    }
  }

  def getNumFiles(groupId: String, artifactId: String, version: String): ZIO[Client, Throwable, Int] = {
    val url = s"https://webjars-file-service.herokuapp.com/numfiles/$groupId/$artifactId/$version"

    defer {
      val response = Client.request(url).run
      response.status match {
        case Status.Ok =>
          val body = response.body.asString.run
          ZIO.attempt(body.toInt).run
        case Status.NotFound =>
          throw new FileNotFoundException(s"Could not get $url - ${response.body}")
        case _ =>
          throw new Exception(s"Error fetching $url : ${response.body}")
      }
    }
  }

}
