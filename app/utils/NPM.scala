package utils

import java.io.InputStream
import java.net.{URI, URL}
import java.util.zip.GZIPInputStream

import play.api.http.{HeaderNames, Status}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class NPM(implicit ec: ExecutionContext, ws: WSClient) {

  val BASE_URL = "http://registry.npmjs.org"

  val licenseUtils = LicenseUtils(ec, ws)
  val git = Git(ec, ws)

  def versions(packageNameOrGitRepo: String): Future[Seq[String]] = {
    if (packageNameOrGitRepo.contains("/")) {
      git.versions(packageNameOrGitRepo)
    }
    else {
      ws.url(s"$BASE_URL/$packageNameOrGitRepo").get().flatMap { response =>
        response.status match {
          case Status.OK =>
            val versions = (response.json \ "versions" \\ "version").map(_.as[String]).sorted(VersionOrdering).reverse
            Future.successful(versions)
          case _ => Future.failed(new Exception(response.body))
        }
      }
    }
  }

  def info(packageNameOrGitRepo: String, maybeVersion: Option[String] = None): Future[PackageInfo] = {

    // todo: fix scm urls if this was a git repo

    def packageInfo(packageJson: JsValue): Future[PackageInfo] = {
      val initialInfo = packageJson.as[PackageInfo](NPM.jsonReads)

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
          licenseUtils.gitHubLicenseDetect(initialInfo.gitHubOrgRepo).map { license =>
            info.copy(licenses = Seq(license))
          } recoverWith {
            case e: Exception =>
              Future.successful(info)
          }
        }
        else {
          Future.successful(info)
        }
      }
    }

    if (packageNameOrGitRepo.contains("/")) {
      versions(packageNameOrGitRepo).flatMap { versions =>
        // if version was set use it, otherwise use the latest version
        val version = maybeVersion.orElse(versions.headOption)
        git.file(packageNameOrGitRepo, version, "package.json").flatMap { packageJsonString =>
          packageInfo(Json.parse(packageJsonString))
        }
      }

    }
    else {
      val url = maybeVersion.fold(s"$BASE_URL/$packageNameOrGitRepo") { version =>
        s"$BASE_URL/$packageNameOrGitRepo/$version"
      }
      ws.url(url).get().flatMap { response =>
        response.status match {
          case Status.OK =>
            packageInfo(response.json)
          case _ =>
            Future.failed(new Exception(response.body))
        }
      }
    }
  }

  def tgz(packageNameOrGitRepo: String, version: String): Future[InputStream] = {
    if (packageNameOrGitRepo.contains("/")) {
      git.tar(packageNameOrGitRepo, Some(version), Set("node_modules"))
    }
    else {
      Future.fromTry {
        Try {
          val url = new URL(s"$BASE_URL/$packageNameOrGitRepo/-/$packageNameOrGitRepo-$version.tgz")
          val inputStream = url.openConnection().getInputStream
          val gzipInputStream = new GZIPInputStream(inputStream)
          gzipInputStream
        }
      }
    }
  }

  def convertNpmDependenciesToMaven(npmDependencies: Map[String, String]): Future[Map[String, String]] = {
    val maybeMavenDeps = npmDependencies.map { case (npmName, npmVersionOrUrl) =>
      if (npmVersionOrUrl.contains("/")) {

        val (url, maybeVersion) = if (npmVersionOrUrl.contains("#")) {
          val parts = npmVersionOrUrl.split("#")
          (parts.head, parts.lastOption)
        }
        else {
          (npmVersionOrUrl, None)
        }

        artifactId(url).flatMap { artifactId =>
          maybeVersion.fold {
            versions(url).map { versions =>
              artifactId -> versions.head
            }
          } { version =>
            Future.successful(artifactId -> version)
          }
        }
      }
      else {
        val maybeMavenVersion = SemVerUtil.convertSemVerToMaven(npmVersionOrUrl)
        maybeMavenVersion.fold {
          Future.failed[(String, String)](new Exception(s"Could not convert npm version to maven for: $npmName $npmVersionOrUrl"))
        } { mavenVersion =>
          Future.successful(npmName -> mavenVersion)
        }
      }
    }

    Future.sequence(maybeMavenDeps).map(_.toMap)
  }

  // converts git urls to unique artifactId's
  def artifactId(nameOrUrlish: String): Future[String] = {
    if (nameOrUrlish.contains("/")) {
      git.gitUrl(nameOrUrlish).flatMap { gitUrl =>
        Future.fromTry {
          Try {
            val uri = new URI(gitUrl.stripSuffix(".git"))

            val host = uri.getHost.replaceAll("[^\\w\\d]", "-")
            val path = uri.getPath.replaceAll("[^\\w\\d]", "-")

            host + path
          }
        }
      }
    }
    else {
      Future.successful(nameOrUrlish)
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
      (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String]))
    )(PackageInfo.apply _)
  }

  def apply(implicit ec: ExecutionContext, ws: WSClient) = new NPM()
}
