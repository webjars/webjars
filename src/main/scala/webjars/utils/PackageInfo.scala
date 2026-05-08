package webjars.utils

import io.lemonlabs.uri.AbsoluteUrl

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
):
  lazy val maybeGitHubUrl: Option[AbsoluteUrl] = GitHub.gitHubUrl(sourceConnectionUri).toOption
    .orElse(maybeHomepageUrl.flatMap(GitHub.gitHubUrl(_).toOption))

case class MissingMetadataException(json: String, errors: Seq[String]) extends Exception:
  override def getMessage: String =
    if errors.length == 1 then
      "The metadata was missing a required field: " + errors.head
    else
      "The metadata was missing required fields:" + errors.mkString(" ")

case class ServerError(message: String, status: Int) extends Exception:
  override def getMessage: String = s"Response was $status - $message"

case class UnauthorizedError(message: String) extends Exception:
  override def getMessage: String = message
