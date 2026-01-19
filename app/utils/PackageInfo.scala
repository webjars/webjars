package utils

import io.lemonlabs.uri.AbsoluteUrl
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

import scala.util.Success

enum LicenseMetadata:
  case SpdxLicense(id: String)
  case ProvidedLicense(license: License)
  case UnresolvedLicense

case class PackageInfo(
                        name: String,
                        version: String,
                        maybeHomepageUrl: Option[AbsoluteUrl],
                        sourceConnectionUri: AbsoluteUrl,
                        maybeIssuesUrl: Option[AbsoluteUrl],
                        metadataLicenses: Seq[LicenseMetadata],
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

  given writesUrl: Writes[AbsoluteUrl] = Writes[AbsoluteUrl](url => JsString(url.toString))

  given readsUrl: Reads[AbsoluteUrl] = Reads[AbsoluteUrl] {
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
