package utils

import java.io.InputStream
import java.net.URL

import play.api.http.{MimeTypes, HeaderNames, Status}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Bower(implicit ec: ExecutionContext, ws: WSClient) {

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

  val licenseUtils = LicenseUtils(ec, ws)
  val git = GitUtil(ec, ws)

  def versions(packageNameOrGitRepo: String): Future[Seq[String]] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap(git.versions)
    }
    else {
      ws.url(s"$BASE_URL/info/$packageNameOrGitRepo").get().flatMap { response =>
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

  def rawInfo(packageNameOrGitRepo: String, version: String): Future[PackageInfo] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap { gitUrl =>
        git.file(gitUrl, Some(version), "bower.json").map { bowerJson =>
          // add the gitUrl into the json since it is not in the file, just the json served by the Bower index
          val json = Json.parse(bowerJson).as[JsObject] + ("_source" -> JsString(gitUrl))
          json.as[PackageInfo](Bower.jsonReads)
        }
      }
    }
    else {
      ws.url(s"$BASE_URL/info/$packageNameOrGitRepo/$version").get().flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful(response.json.as[PackageInfo](Bower.jsonReads))
          case _ =>
            Future.failed(new Exception(response.body))
        }
      }
    }
  }

  def info(packageNameOrGitRepo: String, version: Option[String] = None): Future[PackageInfo] = {

    // if no version was specified use the latest
    val versionFuture: Future[String] = version.fold {
      versions(packageNameOrGitRepo).flatMap { versions =>
        versions.headOption.fold(Future.failed[String](new Exception("The latest version could not be determined.")))(Future.successful)
      }
    } (Future.successful)

    versionFuture.flatMap { version =>

      rawInfo(packageNameOrGitRepo, version).flatMap { initialInfo =>
        // deal with GitHub redirects

        val infoFuture: Future[PackageInfo] = initialInfo.gitHubHome.toOption.fold(Future.successful(initialInfo)) { gitHubHome =>
          ws.url(gitHubHome).withFollowRedirects(false).get().flatMap { homeTestResponse =>
            homeTestResponse.status match {
              case Status.MOVED_PERMANENTLY =>
                homeTestResponse.header(HeaderNames.LOCATION).fold(Future.successful(initialInfo)) { actualHome =>
                  val newSource = actualHome.replaceFirst("https://", "git://") + ".git"
                  Future.successful(initialInfo.copy(sourceUrl = newSource, homepage = actualHome))
                }
              case _ =>
                Future.successful(initialInfo)
            }
          }
        }

        infoFuture.flatMap { info =>
          // detect licenses if they are not specified in the bower.json
          if (info.licenses.isEmpty) {
            licenseUtils.gitHubLicenseDetect(info.gitHubOrgRepo).map { license =>
              info.copy(licenses = Seq(license))
            }
          }
          else {
            val resolvedLicensesFuture = Future.sequence {
              info.licenses.map { license =>
                if (license.contains("/")) {
                  val contentsFuture = if (license.startsWith("http")) {

                    // Some license references point to an HTML GitHub page.  We need the raw text/plain content.
                    val hopefullyTextLicense = if (license.contains("github.com") && license.contains("/blob/")) {
                      license.replaceAllLiterally("/blob/", "/raw/")
                    }
                    else {
                      license
                    }

                    ws.url(hopefullyTextLicense).get().flatMap { response =>
                      response.status match {
                        case Status.OK if response.header(HeaderNames.CONTENT_TYPE).exists(_.startsWith(MimeTypes.TEXT)) => Future.successful(response.body)
                        case Status.OK => Future.failed(new Exception(s"License at $hopefullyTextLicense was not plain text"))
                        case _ => Future.failed(new Exception(s"Could not fetch license at $hopefullyTextLicense - response was: ${response.body}"))
                      }
                    }
                  }
                  else {
                    git.file(info.sourceConnectionUrl, Some(info.version), license)
                  }

                  contentsFuture.flatMap { contents =>
                    licenseUtils.licenseDetect(contents)
                  }
                }
                else {
                  Future.successful(license)
                }
              }
            }

            resolvedLicensesFuture.map { resolvedLicenses =>
              info.copy(licenses = resolvedLicenses)
            }
          }
        }

      }
    }
  }

  def zip(packageNameOrGitRepo: String, version: String): Future[InputStream] = {
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
  implicit def jsonReads: Reads[PackageInfo] = (
    (__ \ "name").read[String] ~
    (__ \ "version").read[String] ~
    (__ \ "homepage").read[String] ~
    (__ \ "_source").read[String].map(_.replace("git://", "https://").stripSuffix(".git")) ~
    (__ \ "_source").read[String] ~
    (__ \ "_source").read[String].map(_.replace("git://", "https://").stripSuffix(".git") + "/issues") ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse(Reads.pure(Seq.empty[String])) ~
    (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String]))
  )(PackageInfo.apply _)

  def apply(implicit ec: ExecutionContext, ws: WSClient) = new Bower()
}

