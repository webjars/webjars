package utils

import java.net.URL

import javax.inject.Inject
import play.api.Logging
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

class LicenseDetector @Inject() (ws: WSClient) (implicit ec: ExecutionContext) extends Logging {

  def licenseDetect(contents: String): Future[LicenseWithName] = {
    ws.url("https://oss-license-detector.herokuapp.com/").post(contents).flatMap { licenseResponse =>
      licenseResponse.status match {
        case Status.OK =>
          Future.successful(LicenseWithName(licenseResponse.body))
        case _ =>
          logger.error("License fetch error:\n" + contents + "\n" + licenseResponse.body)
          Future.failed(new Exception(licenseResponse.body))
      }
    }
  }

  def licenseDetect(url: URL): Future[LicenseWithNameAndUrl] = {
    // sometimes this url points to the license on GitHub which can't be heuristically converted to an actual license so change the url to the raw content
    val rawLicenseUrl = if (url.getHost.endsWith("github.com")) {
      val path = url.getPath.replaceAll("/blob", "")
      new URL(s"https://raw.githubusercontent.com$path")
    }
    else {
      url
    }

    ws.url(rawLicenseUrl.toString).get().flatMap { response =>
      response.status match {
        case Status.OK if response.header(HeaderNames.CONTENT_TYPE).exists(_.startsWith(MimeTypes.TEXT)) =>
          licenseDetect(response.body).map { licenseWithName =>
            LicenseWithNameAndUrl(licenseWithName.name, url)
          }
          // todo: maybe fallback to just URL
        case Status.OK =>
          Future.failed(new Exception(s"License at $url was not plain text"))
        case _ =>
          Future.failed(new Exception(s"Could not fetch license at $url - response was: ${response.body}"))
      }
    }
  }

  val typicalLicenseFiles = Set("LICENSE", "LICENSE.txt", "license.md", "LICENSE-MIT")

}

sealed trait License {
  val maybeName: Option[String] = this match {
    case LicenseWithName(name) => Some(name)
    case _: LicenseWithUrl => None
    case LicenseWithNameAndUrl(name, _) => Some(name)
  }

  val maybeUrl: Option[URL] = this match {
    case _: LicenseWithName => None
    case LicenseWithUrl(url) => Some(url)
    case LicenseWithNameAndUrl(_, url) => Some(url)
  }

  override def toString: String = {
    maybeName
      .orElse(maybeUrl.map(_.toString))
      .getOrElse(throw new Exception("License did not have a name or url"))
  }
}
case class LicenseWithName(name: String) extends License
case class LicenseWithUrl(url: URL) extends License
case class LicenseWithNameAndUrl(name: String, url: URL) extends License

case class LicenseNotFoundException(message: String, cause: Exception = null) extends Exception(message, cause)
case class NoValidLicenses() extends Exception("no valid licenses found")