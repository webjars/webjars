package utils

import java.io.InputStream
import java.net.URL
import java.util.zip.ZipInputStream

import play.api.http.{HeaderNames, Status}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Bower(implicit ec: ExecutionContext, ws: WSClient) {

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

  val licenseUtils = LicenseUtils(ec, ws)

  def all: Future[JsArray] = {
    ws.url("https://bower-component-list.herokuapp.com/").get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json.as[JsArray])
        case _ => Future.failed(new Exception(response.body))
      }
    }
  }

  def info(packageName: String): Future[JsValue] = {
    ws.url(s"$BASE_URL/info/$packageName").get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(response.body))
      }
    }
  }

  def rawInfo(packageName: String, version: String): Future[PackageInfo] = {
    ws.url(s"$BASE_URL/info/$packageName/$version").get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful(response.json.as[PackageInfo](Bower.jsonReads))
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

  def info(packageName: String, version: String): Future[PackageInfo] = {
    rawInfo(packageName, version).flatMap { initialInfo =>
      // deal with GitHub redirects

      val infoFuture: Future[PackageInfo] = initialInfo.gitHubHome.toOption.fold(Future.successful(initialInfo)) { gitHubHome =>
        ws.url(gitHubHome).withFollowRedirects(false).get().flatMap { homeTestResponse =>
          homeTestResponse.status match {
            case Status.MOVED_PERMANENTLY =>
              homeTestResponse.header(HeaderNames.LOCATION).fold(Future.successful(initialInfo)) { actualHome =>
                val newSource = actualHome.replaceFirst("https://", "git://") + ".git"
                Future.successful(initialInfo.copy(sourceUrl = newSource, homepage = actualHome))
              }
            case _ =>
              Future.successful(initialInfo)
          }
        }
      }

      infoFuture.flatMap { info =>
        // detect licenses if they are not specified in the bower.json
        if (info.licenses.length == 0) {
          licenseUtils.gitHubLicenseDetect(info.gitHubOrgRepo).map { license =>
            info.copy(licenses = Seq(license))
          } recoverWith {
            case e: Exception =>
              Future.successful(info)
          }
        }
        else {
          Future.successful(info)
        }
      }
    }
  }

  def zip(packageName: String, version: String): Future[InputStream] = {
    Future.fromTry {
      Try {
        val url = new URL(s"$BASE_URL/download/$packageName/$version")
        url.openConnection().getInputStream
      }
    }
  }

}

object Bower {
  implicit def jsonReads: Reads[PackageInfo] = (
    (__ \ "name").read[String] ~
    (__ \ "version").read[String] ~
    (__ \ "homepage").read[String] ~
    (__ \ "_source").read[String].map(_.replace("git://", "https://").stripSuffix(".git")) ~
    (__ \ "_source").read[String] ~
    (__ \ "_source").read[String].map(_.replace("git://", "https://").stripSuffix(".git") + "/issues") ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse(Reads.pure(Seq.empty[String])) ~
    (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String]))
  )(PackageInfo.apply _)

  def apply(implicit ec: ExecutionContext, ws: WSClient) = new Bower()
}

