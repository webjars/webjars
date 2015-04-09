package utils

import java.io.{InputStream, ByteArrayInputStream}
import java.net.{URL, URI}
import java.util.zip.ZipInputStream

import play.api.http.{HeaderNames, Status}
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Bower(implicit ec: ExecutionContext, ws: WSClient) {

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

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

  def info(packageName: String, version: String): Future[PackageInfo] = {
    ws.url(s"$BASE_URL/info/$packageName/$version").get().flatMap { response =>
      response.status match {
        case Status.OK =>
          // deal with GitHub redirects
          val initialInfo = response.json.as[PackageInfo]
          val infoFuture: Future[PackageInfo] = initialInfo.gitHubHome.toOption.fold(Future.successful(initialInfo)) { gitHubHome =>
            ws.url(gitHubHome).withFollowRedirects(false).get().flatMap { homeTestResponse =>
              homeTestResponse.status match {
                case Status.MOVED_PERMANENTLY =>
                  homeTestResponse.header(HeaderNames.LOCATION).fold(Future.successful(initialInfo)) { actualHome =>
                    val newSource = actualHome.replaceFirst("https://", "git://") + ".git"
                    Future.successful(initialInfo.copy(source = newSource, homepage = actualHome))
                  }
                case _ =>
                  Future.successful(initialInfo)
              }
            }
          }

          infoFuture.flatMap { info =>
            // detect licenses if they are not specified in the bower.json
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

  def zip(packageName: String, version: String): Future[(ZipInputStream, InputStream)] = {
    Future.fromTry {
      Try {
        val url = new URL(s"$BASE_URL/download/$packageName/$version")
        val inputStream = url.openConnection().getInputStream
        val zipInputStream = new ZipInputStream(inputStream)
        (zipInputStream, inputStream)
      }
    }
  }

}

object Bower {
  def apply(implicit ec: ExecutionContext, ws: WSClient) = new Bower()
}

case class PackageInfo(name: String, version: String, homepage: String, source: String, licenses: Seq[String], dependencies: Map[String, String]) {

  lazy val sourceUri: Try[URI] = Try { new URI(source) }
  lazy val gitHubOrg: Try[String] = sourceUri.map(_.getPath.split("/")(1))
  lazy val gitHubRepo: Try[String] = sourceUri.map(_.getPath.split("/")(2).stripSuffix(".git"))
  lazy val gitHubOrgRepo: Try[String] = {
    for {
      org <- gitHubOrg
      repo <- gitHubRepo
    } yield s"$org/$repo"
  }
  lazy val gitHubHome: Try[String] = gitHubOrgRepo.map(orgRepo => s"https://github.com/$orgRepo")
  lazy val issuesUrl: Try[String] = gitHubHome.map(_ + "/issues")

}

object PackageInfo {
  implicit def jsonReads: Reads[PackageInfo] = (
    (__ \ "name").read[String] ~
    (__ \ "version").read[String] ~
    (__ \ "homepage").read[String] ~
    (__ \ "_source").read[String] ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse(Reads.pure(Seq.empty[String])) ~
    (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String]))
  )(PackageInfo.apply _)

  implicit def jsonWrites: Writes[PackageInfo] = Json.writes[PackageInfo]
}