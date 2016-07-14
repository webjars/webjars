package utils

import javax.inject.Inject

import play.api.Logger
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class LicenseDetector @Inject() (ws: WSClient, git: Git, messages: MessagesApi) (implicit ec: ExecutionContext) {

  def gitHubLicenseDetect(maybeGitHubOrgRepo: Try[String]): Future[String] = {
    def fetchLicense(url: String): Future[String] = ws.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.body)
        case _ => Future.failed(LicenseNotFoundException("Could not get license from GitHub", new Exception(response.body)))
      }
    }

    maybeGitHubOrgRepo.map { gitHubOrgRepo =>
      // look on master for a license
      fetchLicense(s"https://github-license-service.herokuapp.com/$gitHubOrgRepo").recoverWith {
        case e: Exception =>
          // look on gh-pages
          fetchLicense(s"https://github-license-service.herokuapp.com/$gitHubOrgRepo/gh-pages")
     }
    } getOrElse Future.failed(LicenseNotFoundException("Could not get license from GitHub"))
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

  def detectLicense(packageInfo: PackageInfo, maybeVersion: Option[String]): Future[PackageInfo] = {

    def fetchLicenseFromRepo(file: String): Future[String] = {
      git.file(packageInfo.sourceConnectionUrl, maybeVersion, file).flatMap(licenseDetect)
    }

    gitHubLicenseDetect(packageInfo.gitHubOrgRepo).recoverWith {
      case e: Exception =>
        fetchLicenseFromRepo("LICENSE").recoverWith {
          case e: Exception =>
            fetchLicenseFromRepo("LICENSE.txt")
        }
    } map { license =>
      packageInfo.copy(licenses = Seq(license))
    } recoverWith {
      case e: Exception =>
        val errorMessage = packageInfo.webJarType match {
          case WebJarType.Bower => messages("licensenotfound.bower")
          case WebJarType.NPM => messages("licensenotfound.npm")
        }
        Future.failed(LicenseNotFoundException(errorMessage, e))
    }
  }

}

case class LicenseNotFoundException(message: String, cause: Exception = null) extends Exception(message, cause)
