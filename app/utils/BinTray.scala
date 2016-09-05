package utils

import java.net.URL
import javax.inject.Inject

import play.api.Configuration
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSAPI, WSAuthScheme, WSRequest, WSResponse}
import play.api.mvc.Results.EmptyContent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class BinTray @Inject() (ws: WSAPI, config: Configuration, git: Git, licenseDetector: LicenseDetector) (implicit ec: ExecutionContext) {

  val BASE_URL = "https://bintray.com/api/v1"

  lazy val username = config.getString("bintray.username").get
  lazy val password = config.getString("bintray.password").get
  lazy val gpgPassphrase = config.getString("bintray.gpg.passphrase").get

  lazy val ossUsername = config.getString("oss.username").get
  lazy val ossPassword = config.getString("oss.password").get


  def ws(path: String): WSRequest = {
    ws
      .url(BASE_URL + path)
      .withAuth(username, password, WSAuthScheme.BASIC)
  }

  def error(response: WSResponse): String = {
    Try {
      (response.json \ "message").as[String]
    } getOrElse response.body
  }

  def createPackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUrl: String, websiteUrl: Option[String], issueTrackerUrl: Option[String], githubRepo: Option[String]): Future[JsValue] = {

    val json = Json.obj(
      "name" -> name,
      "desc" -> desc,
      "labels" -> labels,
      "licenses" -> licenses,
      "vcs_url" -> vcsUrl,
      "website_url" -> websiteUrl,
      "issue_tracker_url" -> issueTrackerUrl,
      "github_repo" -> githubRepo
    )

    ws(s"/packages/$subject/$repo").post(json).flatMap { response =>
      response.status match {
        case Status.CREATED =>
          Future.successful(response.json)
        case _ =>
          Future.failed(new Exception(error(response)))
      }
    }
  }

  def getPackage(subject: String, repo: String, name: String): Future[JsValue] = {
    ws(s"/packages/$subject/$repo/$name").get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(s"Package not found: $subject $repo $name"))
      }
    }
  }

  def getOrCreatePackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUrl: String, websiteUrl: Option[String], issueTrackerUrl: Option[String], githubRepo: Option[String]): Future[JsValue] = {
    getPackage(subject, repo, name).recoverWith {
      case e: Exception =>
        createPackage(subject, repo, name, desc, labels, licenses, vcsUrl, websiteUrl, issueTrackerUrl, githubRepo)
    }
  }

  def deletePackage(subject: String, repo: String, name: String): Future[JsValue] = {
    ws(s"/packages/$subject/$repo/$name").delete().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  def createVersion(subject: String, repo: String, packageName: String, name: String, description: String, vcsTag: Option[String] = None): Future[JsValue] = {
    val json = Json.obj(
      "name" -> name,
      "desc" -> description,
      "vcs_tag" -> vcsTag
    )

    ws(s"/packages/$subject/$repo/$packageName/versions").post(json).flatMap { response =>
      response.status match {
        case Status.CREATED =>
          Future.successful(response.json)
        case Status.CONFLICT =>
          Future.failed(BinTray.VersionExists(error(response)))
        case _ =>
          Future.failed(new Exception(error(response)))
      }
    }
  }

  def uploadMavenArtifact(subject: String, repo: String, packageName: String, path: String, jarBytes: Array[Byte]): Future[JsValue] = {
    ws(s"/maven/$subject/$repo/$packageName/$path").withHeaders("X-GPG-PASSPHRASE" -> gpgPassphrase).put(jarBytes).flatMap { response =>
      response.status match {
        case Status.CREATED => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  def publishVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    val json = Json.obj("publish_wait_for_secs" -> -1)
    ws(s"/content/$subject/$repo/$packageName/$version/publish").post(json).flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  def signVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    ws(s"/gpg/$subject/$repo/$packageName/versions/$version").withHeaders("X-GPG-PASSPHRASE" -> gpgPassphrase).post(EmptyContent()).flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  def syncToMavenCentral(subject: String, repo: String, packageName: String, version:String): Future[JsValue] = {

    val json = Json.obj(
      "username" -> ossUsername,
      "password" -> ossPassword
    )

    ws(s"/maven_central_sync/$subject/$repo/$packageName/versions/$version").post(json).flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

}

object BinTray {
  case class VersionExists(message: String) extends Exception {
    override def getMessage = message
  }
}
