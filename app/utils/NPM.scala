package utils

import java.io.InputStream
import java.net.URL
import java.util.zip.GZIPInputStream
import javax.inject.Inject

import play.api.http.{HeaderNames, Status}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class NPM @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector) (implicit ec: ExecutionContext) {

  val BASE_URL = "http://registry.npmjs.org"

  // a whole lot of WTF
  private def registryMetadataUrl(packageName: String, maybeVersion: Option[String] = None): String = {
    maybeVersion.fold {
      // when a version is not specified an @ must not be encoded
      val encodedPackageName = packageName.replaceAllLiterally("/", "%2F")
      s"$BASE_URL/$encodedPackageName"
    } { version =>
      // when a version is specified an @ must be encoded
      val encodedPackageName = packageName.replaceAllLiterally("/", "%2F").replaceAllLiterally("@", "%40")
      s"$BASE_URL/$encodedPackageName/$version"
    }
  }

  private def isScoped(maybeScopeAndPackageName: String): Boolean = {
    maybeScopeAndPackageName.contains('/') && maybeScopeAndPackageName.startsWith("@")
  }

  private def registryTgzUrl(maybeScopeAndPackageName: String, version: String): String = {
    if (isScoped(maybeScopeAndPackageName)) {
      val parts = maybeScopeAndPackageName.split('/')
      val scope = parts.head
      val packageName = parts.last
      s"$BASE_URL/$scope/$packageName/-/$packageName-$version.tgz"
    }
    else {
      s"$BASE_URL/$maybeScopeAndPackageName/-/$maybeScopeAndPackageName-$version.tgz"
    }
  }

  def versions(packageNameOrGitRepo: String): Future[Seq[String]] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.versions(packageNameOrGitRepo)
    }
    else {
      ws.url(registryMetadataUrl(packageNameOrGitRepo)).get().flatMap { response =>
        response.status match {
          case Status.OK =>
            val versions = (response.json \ "versions").as[Map[String, JsObject]].keys.toIndexedSeq.sorted(VersionOrdering).reverse
            Future.successful(versions)
          case _ => Future.failed(new Exception(response.body))
        }
      }
    }
  }

  def versionsOnBranch(gitRepo: String, branch: String): Future[Seq[String]] = {
    git.gitUrl(gitRepo).flatMap(git.versionsOnBranch(_, branch))
  }

  def info(packageNameOrGitRepo: String, maybeVersion: Option[String] = None): Future[PackageInfo] = {

    def packageInfo(packageJson: JsValue): Future[PackageInfo] = {

      val maybeForkPackageJsonFuture = if (git.isGit(packageNameOrGitRepo)) {
        // this is a git repo so its package.json values might be wrong
        git.gitUrl(packageNameOrGitRepo).map { gitUrl =>
          // replace the repository.url with the possible fork's git url
          val json = packageJson.as[JsObject] ++ Json.obj("repository" -> Json.obj("url" -> gitUrl))

          // todo: resolve differences between json & maybeVersion

          json

        }
      }
      else {
        Future.successful(packageJson)
      }

      maybeForkPackageJsonFuture.flatMap { maybeForPackageJson =>

        maybeForPackageJson.asOpt[PackageInfo](NPM.jsonReads).fold {
          Future.failed[PackageInfo](new Exception(s"The source repository for $packageNameOrGitRepo ${maybeVersion.getOrElse("")} could not be determined but is required to published to Maven Central.  This will need to be fixed in the project's package metadata."))
        } { initialInfo =>

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
            if (info.licenses.isEmpty) {
              // first try to get a license from the source
              licenseDetector.detectLicense(initialInfo, maybeVersion)
            }
            else {
              Future.successful(info)
            }
          }

        }

      }
    }

    if (git.isGit(packageNameOrGitRepo)) {
      versions(packageNameOrGitRepo).flatMap { versions =>
        // if version was set use it, otherwise use the latest version
        val version = maybeVersion.orElse(versions.headOption)
        git.file(packageNameOrGitRepo, version, "package.json").flatMap { packageJsonString =>
          packageInfo(Json.parse(packageJsonString))
        }
      }

    }
    else {
      if (isScoped(packageNameOrGitRepo) && maybeVersion.isDefined) {
        // can no longer get info on specific versions of scoped packages
        // so get the info for all the versions and then get the specific version out of the full list
        ws.url(registryMetadataUrl(packageNameOrGitRepo)).get().flatMap { response =>
          response.status match {
            case Status.OK =>
              val versionInfoLookup = response.json \ "versions" \ maybeVersion.get
              versionInfoLookup.toOption.fold(Future.failed[PackageInfo](new Exception(s"Could not parse: ${response.body}")))(packageInfo)
            case _ =>
              Future.failed(new Exception(response.body))
          }
        }
      }
      else {
        ws.url(registryMetadataUrl(packageNameOrGitRepo, maybeVersion)).get().flatMap { response =>
          response.status match {
            case Status.OK =>
              packageInfo(response.json)
            case _ =>
              Future.failed(new Exception(response.body))
          }
        }
      }
    }
  }

  def tgz(packageNameOrGitRepo: String, version: String): Future[InputStream] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.tar(packageNameOrGitRepo, Some(version), Set("node_modules"))
    }
    else {
      Future.fromTry {
        Try {
          val url = new URL(registryTgzUrl(packageNameOrGitRepo, version))
          val inputStream = url.openConnection().getInputStream
          val gzipInputStream = new GZIPInputStream(inputStream)
          gzipInputStream
        }
      }
    }
  }

}

object NPM {

  implicit def jsonReads: Reads[PackageInfo] = {
    val sourceConnectionUrlReader = (__ \ "repository" \ "url").read[String]

    val sourceUrlReader = sourceConnectionUrlReader.map { sourceConnectionUrl =>
      sourceConnectionUrl.replace("git://", "https://").stripSuffix(".git")
    }

    val licenseReader = (__ \ "license").read[Seq[String]]
      .orElse((__ \ "license").read[String].map(Seq(_)))
      .orElse((__ \ "license").read[JsObject].map(_.\("type").as[String]).map(Seq(_)))
      .orElse((__ \ "licenses").read[Seq[JsObject]].map(_.map(_.\("type").as[String])))
      .orElse(Reads.pure(Seq.empty[String]))

    val bugsReader = (__ \ "bugs" \ "url").read[String]
      .orElse(sourceUrlReader.map(_ + "/issues"))

    (
      (__ \ "name").read[String] ~
      (__ \ "version").read[String] ~
      (__ \ "homepage").read[String].orElse(sourceUrlReader) ~
      sourceUrlReader ~
      sourceConnectionUrlReader ~
      bugsReader ~
      licenseReader ~
      (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
      Reads.pure(WebJarType.NPM)
    )(PackageInfo.apply _)
  }

}
