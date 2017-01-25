package utils

import java.io.InputStream
import java.net.{URI, URL}
import java.util.zip.GZIPInputStream
import javax.inject.Inject

import play.api.http.Status
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsError, _}
import play.api.libs.ws.WSClient
import utils.PackageInfo._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class NPM @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector, gitHub: GitHub) (implicit ec: ExecutionContext) {

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

  def info(packageNameOrGitRepo: String, maybeVersion: Option[String] = None): Future[PackageInfo[NPM]] = {

    def packageInfo(packageJson: JsValue): Future[PackageInfo[NPM]] = {

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

      maybeForkPackageJsonFuture.flatMap { maybeForkPackageJson =>
        maybeForkPackageJson.validate[PackageInfo[NPM]] match {

          case JsSuccess(initialInfo, _) =>
            initialInfo.gitHubUrl.fold(Future.successful(initialInfo)) { gitHubUrl =>
                gitHub.currentUrls(gitHubUrl).map {
                  case (homepage, sourceConnectionUri, issuesUrl) =>
                    initialInfo.copy[NPM](
                      homepageUrl = homepage,
                      sourceUrl = homepage,
                      sourceConnectionUri = sourceConnectionUri,
                      issuesUrl = issuesUrl
                    )
                } recover {
                  case e: Exception =>
                    val newUrl = new URL("https://www.npmjs.com/package/" + initialInfo.name)
                    initialInfo.copy(homepageUrl = newUrl, sourceUrl = newUrl, sourceConnectionUri = newUrl.toURI, issuesUrl = newUrl)
                }
            }

          case JsError(errors) =>
            Future.failed[PackageInfo[NPM]](JsResultException(errors))
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
      if (isScoped(packageNameOrGitRepo)) {
        // can no longer get info on specific versions of scoped packages
        // so get the info for all the versions and then get the specific version out of the full list
        ws.url(registryMetadataUrl(packageNameOrGitRepo)).get().flatMap { response =>
          response.status match {
            case Status.OK =>
              val maybeVersionOrLatest = maybeVersion.orElse((response.json \ "dist-tags" \ "latest").asOpt[String])
              maybeVersionOrLatest.fold(Future.failed[PackageInfo[NPM]](new Exception("Could not determine the version to get"))) { versionOrLatest =>
                val versionInfoLookup = response.json \ "versions" \ versionOrLatest
                versionInfoLookup.toOption.fold(Future.failed[PackageInfo[NPM]](new Exception(s"Could not parse: ${response.body}")))(packageInfo)
              }
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

  def uriIsh(repository: String): String = {
    if (repository.contains("://")) {
      // ssh://host.xz/another/repo.git
      // git://host.xz/another/repo.git
      // https://host.xz/another/repo.git
      repository
    }
    else if (repository.startsWith("gist:")) {
      // gist:11081aaa281
      repository.replaceAllLiterally("gist:", "https://gist.github.com/") + ".git"
    }
    else if (repository.startsWith("bitbucket:")) {
      // bitbucket:example/repo
      repository.replaceAllLiterally("bitbucket:", "https://bitbucket.org/") + ".git"
    }
    else if (repository.startsWith("gitlab:")) {
      // gitlab:another/repo
      repository.replaceAllLiterally("gitlab:", "https://gitlab.com/") + ".git"
    }
    else if (repository.contains(":/")) {
      // host.xz:/another/repo.git
      // user@host.xz:/another/repo.git
      "ssh://" + repository
    }
    else if (repository.contains(":")) {
      // host.xz:another/repo.git
      "ssh://" + repository.replaceAllLiterally(":", "/")
    }
    else if (repository.contains("/")) {
      // another/repo
      "https://github.com/" + repository + ".git"
    }
    else {
      repository
    }
  }

  def repositoryUrlToJsString(repositoryUrl: String): JsString = JsString(uriIsh(repositoryUrl))

  def repositoryToUri(uriIsh: String): Option[URI] = PackageInfo.readsUri.reads(repositoryUrlToJsString(uriIsh)).asOpt

  implicit val jsonReads: Reads[PackageInfo[NPM]] = {
    val repositoryUrlReader: Reads[String] = (__ \ "repository").read[String].orElse((__ \ "repository" \ "url").read[String])

    val sourceConnectionUriReader: Reads[URI] = repositoryUrlReader.map(repositoryUrlToJsString).andThen(PackageInfo.readsUri)

    val sourceUrlReader: Reads[URL] = repositoryUrlReader.map(repositoryUrlToJsString).andThen(PackageInfo.readsUrl)

    val licenseReader = (__ \ "license").read[Seq[String]]
      .orElse((__ \ "license").read[String].map(Seq(_)))
      .orElse((__ \ "license").read[JsObject].map(_.\("type").as[String]).map(Seq(_)))
      .orElse((__ \ "licenses").read[Seq[JsObject]].map(_.map(_.\("type").as[String])))
      .orElse(Reads.pure(Seq.empty[String]))

    val nameReader = (__ \ "name").read[String]

    val homepageReader = (__ \ "homepage").read[URL]
      .orElse(nameReader.map(name => JsString("https://www.npmjs.com/package/" + name))
      .andThen(PackageInfo.readsUrl))

    val bugsReader = (__ \ "bugs" \ "url").read[URL]
      .orElse(homepageReader.flatMap(gitHubIssuesUrl))
      .orElse(sourceUrlReader.flatMap(gitHubIssuesUrl))
      .orElse(sourceConnectionUriReader.flatMap(gitHubIssuesUrl))

    (
      nameReader ~
      (__ \ "version").read[String] ~
      homepageReader ~
      sourceUrlReader.orElse(homepageReader) ~
      sourceConnectionUriReader ~
      bugsReader ~
      licenseReader ~
      (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
      Reads.pure(WebJarType.NPM)
    )(PackageInfo.apply[NPM] _)
  }

  implicit val npmWrites: Writes[NPM] = Writes[NPM](_ => JsNull)

}
