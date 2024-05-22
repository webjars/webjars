package utils

import io.lemonlabs.uri.AbsoluteUrl
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.Success

case class PackageInfo(
                        name: String,
                        version: String,
                        maybeHomepageUrl: Option[AbsoluteUrl],
                        sourceConnectionUri: AbsoluteUrl,
                        maybeIssuesUrl: Option[AbsoluteUrl],
                        metadataLicenses: Seq[String],
                        dependencies: Map[String, String],
                        optionalDependencies: Map[String, String],
                        maybeTag: Option[String],
                      ) {

  lazy val maybeGitHubUrl: Option[AbsoluteUrl] = GitHub.gitHubUrl(sourceConnectionUri).toOption
    .orElse(maybeHomepageUrl.flatMap(GitHub.gitHubUrl(_).toOption))

  /*
  lazy val maybeGitHubOrg: Option[String] = GitHub.maybeGitHubOrg(maybeGitHubUrl)
  lazy val maybeGitHubRepo: Option[String] = GitHub.maybeGitHubRepo(maybeGitHubUrl)
  lazy val maybeGitHubOrgRepo: Option[String] = {
    for {
      org <- maybeGitHubOrg
      repo <- maybeGitHubRepo
    } yield s"$org/$repo"
  }
   */

}

object PackageInfo {

  implicit val writesUrl: Writes[AbsoluteUrl] = Writes[AbsoluteUrl](url => JsString(url.toString))

  implicit def jsonWrites: Writes[PackageInfo] = (
    (__ \ "name").write[String] and
    (__ \ "version").write[String] and
    (__ \ "homepage").writeNullable[AbsoluteUrl] and
    (__ \ "sourceConnectionUri").write[AbsoluteUrl] and
    (__ \ "issuesUrl").writeNullable[AbsoluteUrl] and
    (__ \ "metadataLicenses").write[Seq[String]] and
    (__ \ "dependencies").write[Map[String, String]] and
    (__ \ "optionalDependencies").write[Map[String, String]] and
    (__ \ "tag").writeNullable[String]
  )(unlift(PackageInfo.unapply))

  implicit val readsUrl: Reads[AbsoluteUrl] = Reads[AbsoluteUrl] {
    case JsString(s) =>
      AbsoluteUrl.parseTry(s) match {
        case Success(url) => JsSuccess(url)
        case _ => JsError(JsonValidationError(s"Could not convert $s to a URL"))
      }
    case _ =>
      JsError(JsonValidationError("Could not read the URL as a string"))
  }

}

case class MissingMetadataException(json: JsValue, errors: collection.Seq[(JsPath, collection.Seq[JsonValidationError])]) extends Exception {
  override def getMessage: String = {
    if (errors.length == 1) {
      "The metadata was missing a required field: " + errors.head._1.path.mkString
    }
    else {
      "The metadata was missing required fields:" + errors.map(_._1.path.mkString).mkString(" ")
    }
  }
}
