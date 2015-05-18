package utils

import java.io.InputStream
import java.net.URL
import java.util.zip.GZIPInputStream

import play.api.http.Status
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class NPM(implicit ec: ExecutionContext, ws: WSClient) {

  val BASE_URL = "http://registry.npmjs.org"

  val licenseUtils = LicenseUtils(ec, ws)

  def latest(packageName: String): Future[JsValue] = {
    ws.url(s"$BASE_URL/$packageName/latest").get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(response.body))
      }
    }
  }

  def info(packageName: String): Future[JsValue] = {
    ws.url(s"$BASE_URL/$packageName").get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(response.body))
      }
    }
  }

  def info(packageName: String, version: String): Future[PackageInfo] = {
    ws.url(s"$BASE_URL/$packageName/$version").get().flatMap { response =>
      response.status match {
        case Status.OK =>
          val initialInfo = response.json.as[PackageInfo](NPM.jsonReads)

          if (initialInfo.licenses.length == 0) {
            licenseUtils.gitHubLicenseDetect(initialInfo.gitHubOrgRepo).map { license =>
              initialInfo.copy(licenses = Seq(license))
            } recoverWith {
              case e: Exception =>
                Future.successful(initialInfo)
            }
          }
          else {
            Future.successful(initialInfo)
          }
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

  def tgz(packageName: String, version: String): Future[InputStream] = {
    Future.fromTry {
      Try {
        val url = new URL(s"$BASE_URL/$packageName/-/$packageName-$version.tgz")
        val inputStream = url.openConnection().getInputStream
        val gzipInputStream = new GZIPInputStream(inputStream)
        gzipInputStream
      }
    }
  }

}

object NPM {

  implicit def jsonReads: Reads[PackageInfo] = {
    val sourceConnectionUrlReader = (__ \ "repository" \ "url").read[String]
    val sourceUrlReader = sourceConnectionUrlReader.map { sourceConnectionUrl =>
      sourceConnectionUrl.stripPrefix("git+").stripSuffix(".git")
    }

    (
      (__ \ "name").read[String] ~
      (__ \ "version").read[String] ~
      (__ \ "homepage").read[String] ~
      sourceUrlReader ~
      sourceConnectionUrlReader ~
      (__ \ "bugs" \ "url").read[String] ~
      (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse(Reads.pure(Seq.empty[String])) ~
      (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String]))
    )(PackageInfo.apply _)
  }

  def apply(implicit ec: ExecutionContext, ws: WSClient) = new NPM()
}