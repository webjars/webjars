package utils

import java.net.{URI, URL}

import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsError, _}

import scala.util.{Failure, Success, Try}

case class PackageInfo[A](name: String, version: String, homepageUrl: URL, sourceUrl: URL, sourceConnectionUri: URI, issuesUrl: URL, metadataLicenses: Seq[String], dependencies: Map[String, String], webJarType: WebJarType.Value) {

  lazy val gitHubUrl: Option[URL] = GitHub.gitHubUrl(homepageUrl)
    .orElse(GitHub.gitHubUrl(sourceUrl))
    .orElse(GitHub.gitHubUrl(sourceConnectionUri)).toOption

  lazy val gitHubOrg: Option[String] = gitHubUrl.map(_.getPath.split("/")(1))
  lazy val gitHubRepo: Option[String] = gitHubUrl.map(_.getPath.split("/")(2).stripSuffix(".git"))
  lazy val gitHubOrgRepo: Option[String] = {
    for {
      org <- gitHubOrg
      repo <- gitHubRepo
    } yield s"$org/$repo"
  }

}

object WebJarType extends Enumeration {
  val NPM, Bower = Value
}

object PackageInfo {

  implicit val writesUrl: Writes[URL] = Writes[URL](url => JsString(url.toString))

  implicit val writesUri: Writes[URI] = Writes[URI](uri => JsString(uri.toString))

  implicit def jsonWrites[T](implicit nested: Writes[T]): Writes[PackageInfo[T]] = (
    (__ \ "name").write[String] and
    (__ \ "version").write[String] and
    (__ \ "homepage").write[URL] and
    (__ \ "sourceUrl").write[URL] and
    (__ \ "sourceConnectionUri").write[URI] and
    (__ \ "issuesUrl").write[URL] and
    (__ \ "metadataLicenses").write[Seq[String]] and
    (__ \ "dependencies").write[Map[String, String]] and
    (__ \ "webJarType").write[WebJarType.Value]
  )(unlift(PackageInfo.unapply[T]))

  implicit val readsUrl: Reads[URL] = Reads[URL] {
    case JsString(s) =>
      Try(new URL(s)) match {
        case Success(url) => JsSuccess(url)
        case _ => JsError(ValidationError(s"Could not convert $s to a URL"))
      }
    case _ =>
      JsError(ValidationError("Could not read the URL as a string"))
  }

  implicit val readsUri: Reads[URI] = Reads[URI] {
    case JsString(s) =>
      Try(new URI(s)) match {
        case Success(url) => JsSuccess(url)
        case _ => JsError(ValidationError(s"Could not convert $s to a URI"))
      }
    case _ =>
      JsError(ValidationError("Could not read the URI as a string"))
  }

  def gitHubUrl(uri: URI): Reads[URL] = Reads[URL] { _ =>
    GitHub.gitHubUrl(uri) match {
      case Success(gitHubUrl) => JsSuccess(gitHubUrl)
      case Failure(e) => JsError(e.getMessage)
    }
  }

  def gitHubIssuesUrl(url: URL): Reads[URL] = Reads[URL] { _ =>
    GitHub.gitHubIssuesUrl(url) match {
      case Success(gitHubIssuesUrl) => JsSuccess(gitHubIssuesUrl)
      case Failure(e) => JsError(e.getMessage)
    }
  }

  def gitHubIssuesUrl(uri: URI): Reads[URL] = Reads[URL] { _ =>
    GitHub.gitHubIssuesUrl(uri) match {
      case Success(gitHubIssuesUrl) => JsSuccess(gitHubIssuesUrl)
      case Failure(e) => JsError(e.getMessage)
    }
  }

}
