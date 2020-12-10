package utils

import java.net.{URI, URL}

import com.google.inject.ImplementedBy
import javax.inject.Inject
import play.api.Configuration
import play.api.http.Status
import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.libs.ws.{WSAuthScheme, WSClient, WSRequest, WSResponse}
import play.mvc.Http.HttpVerbs

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@ImplementedBy(classOf[BinTrayLive])
trait BinTray {
  def createPackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUri: URI, websiteUrl: Option[URL], issueTrackerUrl: Option[URL], githubRepo: Option[String]): Future[JsValue]
  def getPackages(subject: String, repo: String): Future[JsArray]
  def getOrCreatePackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUri: URI, websiteUrl: Option[URL], issueTrackerUrl: Option[URL], githubRepo: Option[String]): Future[JsValue]
  def createVersion(subject: String, repo: String, packageName: String, version: String, description: String, vcsTag: Option[String] = None): Future[JsValue]
  def createOrOverwriteVersion(subject: String, repo: String, packageName: String, version: String, description: String, vcsTag: Option[String] = None): Future[JsValue]
  def deletePackage(subject: String, repo: String, name: String): Future[JsValue]
  def uploadMavenArtifact(subject: String, repo: String, packageName: String, path: String, jarBytes: Array[Byte]): Future[JsValue]
  def signVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue]
  def publishVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue]
  def syncToMavenCentral(subject: String, repo: String, packageName: String, version:String): Future[JsValue]
}

class BinTrayLive @Inject() (ws: WSClient, config: Configuration, mavenCentral: MavenCentral) (implicit ec: ExecutionContext) extends BinTray {

  val BASE_URL = "https://bintray.com/api/v1"

  lazy val username: String = config.get[String]("bintray.username")
  lazy val password: String = config.get[String]("bintray.password")
  lazy val gpgPassphrase: String = config.get[String]("bintray.gpg.passphrase")

  def ws(path: String): WSRequest = {
    ws.url(BASE_URL + path).withAuth(username, password, WSAuthScheme.BASIC)
  }

  def error(response: WSResponse): String = {
    Try((response.json \ "message").as[String]).getOrElse(response.body)
  }

  override def createPackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUri: URI, websiteUrl: Option[URL], issueTrackerUrl: Option[URL], githubRepo: Option[String]): Future[JsValue] = {

    val json = Json.obj(
      "name" -> name,
      "desc" -> desc,
      "labels" -> labels,
      "licenses" -> licenses,
      "vcs_url" -> vcsUri.toString,
      "website_url" -> websiteUrl.map(_.toString),
      "issue_tracker_url" -> issueTrackerUrl.map(_.toString),
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

  override def getPackages(subject: String, repo: String): Future[JsArray] = {
    ws(s"/repos/$subject/$repo/packages").get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json.as[JsArray])
        case _ => Future.failed(new Exception(error(response)))
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

  override def getOrCreatePackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUri: URI, websiteUrl: Option[URL], issueTrackerUrl: Option[URL], githubRepo: Option[String]): Future[JsValue] = {
    getPackage(subject, repo, name).recoverWith {
      case _: Exception =>
        createPackage(subject, repo, name, desc, labels, licenses, vcsUri, websiteUrl, issueTrackerUrl, githubRepo)
    }
  }

  override def deletePackage(subject: String, repo: String, name: String): Future[JsValue] = {
    ws(s"/packages/$subject/$repo/$name").delete().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  override def createVersion(subject: String, repo: String, packageName: String, version: String, description: String, vcsTag: Option[String] = None): Future[JsValue] = {
    val json = Json.obj(
      "name" -> version,
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

  def deleteVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    ws(s"/packages/$subject/$repo/$packageName/versions/$version").delete().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful(response.json)
        case _ =>
          Future.failed(new Exception(error(response)))
      }
    }
  }

  override def createOrOverwriteVersion(subject: String, repo: String, packageName: String, version: String, description: String, vcsTag: Option[String] = None): Future[JsValue] = {
    createVersion(subject, repo, packageName, version, description, vcsTag).recoverWith {
      case _: BinTray.VersionExists =>
        deleteVersion(subject, repo, packageName, version).flatMap { _ =>
          createVersion(subject, repo, packageName, version, description, vcsTag)
        }
    }
  }

  override def uploadMavenArtifact(subject: String, repo: String, packageName: String, path: String, jarBytes: Array[Byte]): Future[JsValue] = {
    ws(s"/maven/$subject/$repo/$packageName/$path").withHttpHeaders("X-GPG-PASSPHRASE" -> gpgPassphrase).put(jarBytes).flatMap { response =>
      response.status match {
        case Status.CREATED => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  override def publishVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    val json = Json.obj("publish_wait_for_secs" -> -1)
    ws(s"/content/$subject/$repo/$packageName/$version/publish").post(json).flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  override def signVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    ws(s"/gpg/$subject/$repo/$packageName/versions/$version").withHttpHeaders("X-GPG-PASSPHRASE" -> gpgPassphrase).execute(HttpVerbs.POST).flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(new Exception(error(response)))
      }
    }
  }

  override def syncToMavenCentral(subject: String, repo: String, packageName: String, version:String): Future[JsValue] = {
    if (mavenCentral.disableDeploy) {
      Future.failed(new Exception("Deployment to Maven Central Disabled"))
    }
    else {
      mavenCentral.withOssCredentials { (ossUsername, ossPassword) =>
        val json = Json.obj(
          "username" -> ossUsername,
          "password" -> ossPassword
        )

        ws(s"/maven_central_sync/$subject/$repo/$packageName/versions/$version").withRequestTimeout(5.minutes).post(json).flatMap { response =>
          response.status match {
            case Status.OK => Future.successful(response.json)
            case _ => Future.failed(new Exception(error(response)))
          }
        }
      }
    }
  }

}

object BinTray {
  case class VersionExists(message: String) extends Exception {
    override def getMessage: String = message
  }
}
