package utils

import play.api.Logger
import play.api.http.Status
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class LicenseUtils(implicit ec: ExecutionContext, ws: WSClient) {

  def gitHubLicenseDetect(maybeGitHubOrgRepo: Try[String]): Future[String] = {
    def fetchLicense(url: String): Future[String] = ws.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.body)
        case _ => Future.failed(new LicenseNotFoundException("Could not get license from GitHub"))
      }
    }

    maybeGitHubOrgRepo.map { gitHubOrgRepo =>
      // look on master for a license
      fetchLicense(s"https://github-license-service.herokuapp.com/$gitHubOrgRepo").recoverWith {
        case e: Exception =>
          // look on gh-pages
          fetchLicense(s"https://github-license-service.herokuapp.com/$gitHubOrgRepo/gh-pages")
     }
    }.getOrElse(Future.failed(new LicenseNotFoundException("Could not get license from GitHub")))
  }

  def licenseDetect(contents: String): Future[String] = {
    ws.url("https://oss-license-detector.herokuapp.com/").post(contents).flatMap { licenseResponse =>
      licenseResponse.status match {
        case Status.OK =>
          Future.successful(licenseResponse.body)
        case _ =>
          Logger.error("License fetch error:\n" + contents + "\n" + licenseResponse.body)
          Future.failed(new Exception(licenseResponse.body))
      }
    }
  }

}

case class LicenseNotFoundException(message: String) extends Exception {
  override def getMessage = message
}

object LicenseUtils {
  def apply(implicit ec: ExecutionContext, ws: WSClient) = new LicenseUtils()
}