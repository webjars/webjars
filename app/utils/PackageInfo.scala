package utils

import java.net.{URI, URL}

import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

case class PackageInfo[A](name: String, version: String, maybeHomepageUrl: Option[URL], sourceConnectionUri: URI, maybeIssuesUrl: Option[URL], metadataLicenses: Seq[String], dependencies: Map[String, String], optionalDependencies: Map[String, String]) {

  // todo: right now we can't reliably derive a sourceUrl (web interface to SCM) so just use maybeGitHubUrl instead

  lazy val maybeGitHubUrl: Option[URL] = GitHub.gitHubUrl(sourceConnectionUri).toOption
    .orElse(maybeHomepageUrl.flatMap(GitHub.gitHubUrl(_).toOption))

  lazy val maybeGitHubOrg: Option[String] = maybeGitHubUrl.map(_.getPath.split("/")(1))
  lazy val maybeGitHubRepo: Option[String] = maybeGitHubUrl.map(_.getPath.split("/")(2).stripSuffix(".git"))
  lazy val maybeGitHubOrgRepo: Option[String] = {
    for {
      org <- maybeGitHubOrg
      repo <- maybeGitHubRepo
    } yield s"$org/$repo"
  }

}

object PackageInfo {

  implicit val writesUrl: Writes[URL] = Writes[URL](url => JsString(url.toString))

  implicit val writesUri: Writes[URI] = Writes[URI](uri => JsString(uri.toString))

  implicit def jsonWrites[T]: Writes[PackageInfo[T]] = (
    (__ \ "name").write[String] and
    (__ \ "version").write[String] and
    (__ \ "homepage").writeNullable[URL] and
    (__ \ "sourceConnectionUri").write[URI] and
    (__ \ "issuesUrl").writeNullable[URL] and
    (__ \ "metadataLicenses").write[Seq[String]] and
    (__ \ "dependencies").write[Map[String, String]] and
    (__ \ "optionalDependencies").write[Map[String, String]]
  )(unlift(PackageInfo.unapply[T]))

  implicit val readsUrl: Reads[URL] = Reads[URL] {
    case JsString(s) =>
      Try(new URL(s)) match {
        case Success(url) => JsSuccess(url)
        case _ => JsError(JsonValidationError(s"Could not convert $s to a URL"))
      }
    case _ =>
      JsError(JsonValidationError("Could not read the URL as a string"))
  }

  implicit val readsUri: Reads[URI] = Reads[URI] {
    case JsString(s) =>
      Try(new URI(s)) match {
        case Success(uri) => JsSuccess(uri)
        case _ => JsError(JsonValidationError(s"Could not convert $s to a URI"))
      }
    case _ =>
      JsError(JsonValidationError("Could not read the URI as a string"))
  }

}

case class MissingMetadataException(json: JsValue, errors: Seq[(JsPath, Seq[JsonValidationError])]) extends Exception {
  override def getMessage: String = {
    if (errors.length == 1) {
      "The metadata was missing a required field: " + errors.head._1.path.mkString
    }
    else {
      "The metadata was missing required fields:" + errors.map(_._1.path.mkString).mkString(" ")
    }
  }
}
