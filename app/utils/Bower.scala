package utils

import java.io.ByteArrayInputStream
import java.net.URI
import java.util.zip.ZipInputStream

import play.api.http.Status
import play.api.libs.iteratee.Iteratee

import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Bower(implicit ec: ExecutionContext, ws: WSAPI) {

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

  def info(packageName: String, version: String): Future[PackageInfo] = {
    ws.url(s"$BASE_URL/info/$packageName/$version").get().flatMap { response =>
      response.status match {
        case Status.OK =>
          val info = response.json.as[PackageInfo]
          if (info.licenses.length == 0) {
            gitHubLicenseDetect(info).map { license =>
              info.copy(licenses = Seq(license))
            } recoverWith {
              case e: Exception =>
                Future.successful(info)
            }
          }
          else {
            Future.successful(info)
          }
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

  def gitHubLicenseDetect(info: PackageInfo): Future[String] = {
    val tryLicense = for {
      org <- info.gitHubOrg
      repo <- info.gitHubRepo
    } yield {
      ws.url("https://github-license-service.herokuapp.com/twbs/bootstrap").get().flatMap { response =>
        response.status match {
          case Status.OK => Future.successful(response.body)
          case _ => Future.failed(new Exception("Could not get license"))
        }
      }
    }
    tryLicense.getOrElse(Future.failed(new Exception("Could not get license")))
  }

  def zip(packageName: String, version: String): Future[ZipInputStream] = {
    ws.url(s"$BASE_URL/download/$packageName/$version").getStream().flatMap {
      case (headers, enumerator) =>

        val bodyFuture = enumerator |>>> Iteratee.fold(Array.empty[Byte])(_ ++ _)

        bodyFuture.map { rawBody =>
          println(rawBody.length)
          new ZipInputStream(new ByteArrayInputStream(rawBody))
        }
    }
  }

}

object Bower {
  def apply(implicit ec: ExecutionContext, ws: WSAPI) = new Bower()
}

case class PackageInfo(artifactId: String, version: String, homepage: String, source: String, licenses: Seq[String]) {

  lazy val sourceUri: Try[URI] = Try { new URI(source) }
  lazy val gitHubOrg: Try[String] = sourceUri.map(_.getPath.split("/")(1))
  lazy val gitHubRepo: Try[String] = sourceUri.map(_.getPath.split("/")(2).stripSuffix(".git"))

}

object PackageInfo {
  implicit def jsonReads: Reads[PackageInfo] = (
    (__ \ "name").read[String] ~
    (__ \ "version").read[String] ~
    (__ \ "homepage").read[String] ~
    (__ \ "_source").read[String] ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse(Reads.pure(Seq.empty[String]))
  )(PackageInfo.apply _)
}