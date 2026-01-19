package utils

import play.api.http.Status
import play.api.libs.ws.WSClient
import play.api.libs.ws.DefaultBodyReadables.*

import java.io.FileNotFoundException
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class WebJarsFileService @Inject() (ws: WSClient) (implicit ec: ExecutionContext) {

  def getFileList(groupId: String, artifactId: String, version: String): Future[List[String]] = {
    val url = s"https://webjars-file-service.herokuapp.com/listfiles/$groupId/$artifactId/$version"
    ws.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          response.json.asOpt[List[String]].fold(Future.failed[List[String]](new Exception("")))(Future.successful)
        case Status.NOT_FOUND =>
          Future.failed(new FileNotFoundException(s"Could not get $url - ${response.body[String]}"))
        case _ =>
          Future.failed(new Exception(s"Error fetching $url : ${response.body[String]}"))
      }
    }
  }

  def getNumFiles(groupId: String, artifactId: String, version: String): Future[Int] = {
    val url = s"https://webjars-file-service.herokuapp.com/numfiles/$groupId/$artifactId/$version"
    ws.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.fromTry(Try(response.body[String].toInt))
        case Status.NOT_FOUND =>
          Future.failed(new FileNotFoundException(s"Could not get $url - ${response.body[String]}"))
        case _ =>
          Future.failed(new Exception(s"Error fetching $url : ${response.body[String]}"))
      }
    }
  }

}
