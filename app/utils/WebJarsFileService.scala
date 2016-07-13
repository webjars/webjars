package utils

import java.io.FileNotFoundException
import javax.inject.Inject

import play.api.http.Status
import play.api.libs.ws.WSClient
import shade.memcached.MemcachedCodecs._

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class WebJarsFileService @Inject() (memcache: Memcache, ws: WSClient) (implicit ec: ExecutionContext) {

  private def fetchFileList(groupId: String, artifactId: String, version: String): Future[List[String]] = {
    val url = s"http://webjars-file-service.herokuapp.com/listfiles/$groupId/$artifactId/$version"
    ws.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          response.json.asOpt[List[String]].fold(Future.failed[List[String]](new Exception("")))(Future.successful)
        case Status.NOT_FOUND =>
          Future.failed(new FileNotFoundException(s"Could not get $url - ${response.body}"))
        case _ =>
          Future.failed(new Exception(s"Error fetching $url : ${response.body}"))
      }
    }
  }

  def getFileList(groupId: String, artifactId: String, version: String): Future[List[String]] = {
    val cacheKey =  groupId + "-" + artifactId + "-" + version + "-files"

    def fetchAndCache = {
      val fileListFuture = fetchFileList(groupId, artifactId, version)
      fileListFuture.foreach { fileList =>
        memcache.instance.set(cacheKey, fileList, Duration.Inf)
      }
      fileListFuture
    }

    memcache.instance.get[List[String]](cacheKey).flatMap { maybeFileList =>
      maybeFileList.fold(fetchAndCache)(Future.successful)
    } recoverWith {
      case e: Exception => fetchAndCache
    }
  }

  def getNumFiles(groupId: String, artifactId: String, version: String): Future[Int] = {
    val url = s"http://webjars-file-service.herokuapp.com/numfiles/$groupId/$artifactId/$version"
    ws.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.fromTry(Try(response.body.toInt))
        case Status.NOT_FOUND =>
          Future.failed(new FileNotFoundException(s"Could not get $url - ${response.body}"))
        case _ =>
          Future.failed(new Exception(s"Error fetching $url : ${response.body}"))
      }
    }
  }

}
