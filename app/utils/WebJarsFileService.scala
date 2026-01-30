package utils

import com.jamesward.zio_mavencentral.MavenCentral
import play.api.Configuration
import play.api.http.Status
import play.api.libs.ws.DefaultBodyReadables.*
import play.api.libs.ws.WSClient

import java.io.FileNotFoundException
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class WebJarsFileService @Inject() (ws: WSClient, config: Configuration) (using ec: ExecutionContext) {

  private val baseUrl = config.get[String]("webjars.file-service.url")

  def getFileList(gav: MavenCentral.GroupArtifactVersion): Future[List[String]] = {
    val url = s"$baseUrl/listfiles/${gav.toPath.dropLeadingSlash}"
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

  def getNumFiles(gav: MavenCentral.GroupArtifactVersion): Future[Int] = {
    val url = s"$baseUrl/numfiles/${gav.toPath.dropLeadingSlash}"
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
