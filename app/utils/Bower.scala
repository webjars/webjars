package utils

import java.io.InputStream
import java.net.{URI, URL, URLEncoder}
import javax.inject.Inject

import play.api.http.Status
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import utils.PackageInfo._

class Bower @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector, gitHub: GitHub) (implicit ec: ExecutionContext) {

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

  def versions(packageNameOrGitRepo: String): Future[Seq[String]] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap(git.versions)
    }
    else {
      val maybeName = Try {
        URLEncoder.encode(packageNameOrGitRepo, "UTF-8")
      }

      maybeName.toOption.fold[Future[Seq[String]]] {
        Future.failed(new Exception("Could not encode the URL for the specified package"))
      } { name =>
        ws.url(s"$BASE_URL/info/$name").get().flatMap { response =>
          response.status match {
            case Status.OK =>
              val versions = (response.json \ "versions").as[Seq[String]]
              val cleanVersions = versions.filterNot(_.contains("sha"))
              Future.successful(cleanVersions)
            case _ =>
              Future.failed(new Exception(response.body))
          }
        }
      }
    }
  }

  def versionsOnBranch(gitRepo: String, branch: String): Future[Seq[String]] = {
    git.gitUrl(gitRepo).flatMap(git.versionsOnBranch(_, branch))
  }

  def rawInfo(packageNameOrGitRepo: String, version: String): Future[PackageInfo[Bower]] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap { gitUrl =>
        git.file(gitUrl, Some(version), "bower.json").map { bowerJson =>
          // add the gitUrl into the json since it is not in the file, just the json served by the Bower index
          val json = Json.parse(bowerJson).as[JsObject] + ("_source" -> JsString(gitUrl))

          val jsonWithCorrectVersion = (json \ "version").asOpt[String].fold {
            // the version was not in the json so add the specified version
            json + ("version" -> JsString(version))
          } { version =>
            // todo: resolve conflicts?
            // for now just use the version from the json
            json
          }

          jsonWithCorrectVersion.as[PackageInfo[Bower]](Bower.jsonReads)
        }
      }
    }
    else {
      ws.url(s"$BASE_URL/info/$packageNameOrGitRepo/$version").get().flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful(response.json.as[PackageInfo[Bower]](Bower.jsonReads))
          case _ =>
            Future.failed(new Exception(response.body))
        }
      }
    }
  }

  def info(packageNameOrGitRepo: String, maybeVersion: Option[String] = None): Future[PackageInfo[Bower]] = {

    // if no version was specified use the latest
    val versionFuture: Future[String] = maybeVersion.fold {
      versions(packageNameOrGitRepo).flatMap { versions =>
        versions.headOption.fold(Future.failed[String](new Exception("The latest version could not be determined.")))(Future.successful)
      }
    }(Future.successful)

    versionFuture.flatMap { version =>
      rawInfo(packageNameOrGitRepo, version).flatMap { initialInfo =>
        initialInfo.gitHubUrl.fold(Future.successful(initialInfo)) { gitHubUrl =>
          gitHub.currentUrls(gitHubUrl).map {
            case (homepage, sourceConnectionUri, issuesUrl) =>
              initialInfo.copy[Bower](
                homepageUrl = homepage,
                sourceUrl = homepage,
                sourceConnectionUri = sourceConnectionUri,
                issuesUrl = issuesUrl
              )

          }
        }
      }
    }
  }

  def archive(packageNameOrGitRepo: String, version: String): Future[InputStream] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap { gitUrl =>
        git.tar(gitUrl, Some(version), Set("bower_modules"))
      }
    }
    else {
      Future.fromTry {
        Try {
          val url = new URL(s"$BASE_URL/download/$packageNameOrGitRepo/$version")
          url.openConnection().getInputStream
        }
      }
    }
  }

}

object Bower {
  val sourceReads: Reads[URI] = (__ \ "_source").read[URI]

  implicit def jsonReads: Reads[PackageInfo[Bower]] = (
    (__ \ "name").read[String] ~
    (__ \ "version").read[String] ~
    (__ \ "homepage").read[URL].orElse(sourceReads.flatMap(PackageInfo.gitHubUrl)) ~
    sourceReads.flatMap(PackageInfo.gitHubUrl) ~
    sourceReads ~
    sourceReads.flatMap(PackageInfo.gitHubIssuesUrl) ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse(Reads.pure(Seq.empty[String])) ~
    (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
    Reads.pure(Map.empty[String, String])
  )(PackageInfo.apply[Bower] _)

  val groupId: String = "org.webjars.bower"

  def deployable(bower: Bower): Deployable[Bower] = new Deployable[Bower] {
    override val name: String = "Bower"

    override val groupId: String = Bower.groupId

    override val excludes: Set[String] = Set(".bower.json")

    override val metadataFile: String = "bower.json"

    override val contentsInSubdir: Boolean = false

    override def archive(nameOrUrlish: String, version: String): Future[InputStream] = bower.archive(nameOrUrlish, version)

    override def info(nameOrUrlish: String, maybeVersion: Option[String]): Future[PackageInfo[Bower]] = bower.info(nameOrUrlish, maybeVersion)
  }
}

