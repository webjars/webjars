package utils

import play.api.http.Status
import play.api.i18n.{Langs, MessagesApi}
import play.api.libs.concurrent.Futures
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.WSClient
import utils.Deployable.{NameOrUrlish, Version}
import utils.PackageInfo._

import java.io.InputStream
import java.net.{URI, URL}
import java.util.zip.{GZIPInputStream, ZipException}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class NPM @Inject() (val ws: WSClient, val licenseDetector: LicenseDetector, val messages: MessagesApi, val langs: Langs, git: Git, gitHub: GitHub, maven: Maven, semVer: SemVer)(implicit ec: ExecutionContext) extends Deployable {

  val BASE_URL = "https://registry.npmjs.org"

  override val name: String = "NPM"

  override val groupIdQuery: String = "org.webjars.npm"

  override def includesGroupId(groupId: String): Boolean = groupId.equalsIgnoreCase(groupIdQuery)

  override def groupId(nameOrUrlish: NameOrUrlish, version: Version): Future[String] = Future.successful(groupIdQuery)

  override def artifactId(nameOrUrlish: NameOrUrlish, version: Version): Future[String] = git.artifactId(nameOrUrlish)

  override def excludes(nameOrUrlish: NameOrUrlish, version: Version): Future[Set[String]] = {
    // todo: apply npm ignore in case of git repo
    Future.successful(Set("node_modules"))
  }

  override val metadataFile: String = "package.json"

  override val contentsInSubdir: Boolean = true

  override def pathPrefix(nameOrUrlish: NameOrUrlish, releaseVersion: Version, packageInfo: PackageInfo): Future[String] = {
    artifactId(nameOrUrlish, releaseVersion).map { artifactId =>
      s"$artifactId/$releaseVersion/"
    }
  }

  // a whole lot of WTF
  private def registryMetadataUrl(packageName: String, maybeVersion: Option[Version] = None): String = {
    maybeVersion.fold {
      // when a version is not specified an @ must not be encoded
      val encodedPackageName = packageName.replace("/", "%2F")
      s"$BASE_URL/$encodedPackageName"
    } { version =>
      // when a version is specified an @ must be encoded
      val encodedPackageName = packageName.replace("/", "%2F").replace("@", "%40")
      s"$BASE_URL/$encodedPackageName/$version"
    }
  }

  private def isScoped(maybeScopeAndPackageName: String): Boolean = {
    maybeScopeAndPackageName.contains('/') && maybeScopeAndPackageName.startsWith("@")
  }

  private def registryTgzUrl(maybeScopeAndPackageName: String, version: Version): String = {
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

  override def versions(packageNameOrGitRepo: NameOrUrlish): Future[Set[Version]] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.versions(packageNameOrGitRepo)
    }
    else {
      ws.url(registryMetadataUrl(packageNameOrGitRepo)).get().flatMap { response =>
        response.status match {
          case Status.OK =>
            val versions = (response.json \ "versions").as[Map[String, JsObject]].keys.toSet
            Future.successful(versions)
          case _ => Future.failed(new Exception(response.body))
        }
      }
    }
  }

  override def info(packageNameOrGitRepo: NameOrUrlish, version: Version, maybeSourceUri: Option[URI] = None): Future[PackageInfo] = {

    def packageInfo(packageJson: JsValue): Future[PackageInfo] = {

      val maybeForkPackageJsonFuture = if (git.isGit(packageNameOrGitRepo)) {
        // this is a git repo so its package.json values might be wrong
        git.gitUrl(packageNameOrGitRepo).map { gitUrl =>
          // replace the repository.url with the possible fork's git url
          packageJson.as[JsObject] ++ Json.obj("repository" -> Json.obj("url" -> gitUrl))
        }
      }
      else {
        Future.successful(packageJson)
      }

      maybeForkPackageJsonFuture.flatMap { maybeForkPackageJson =>

        // allow override of the source uri
        val maybeSourceOverridePackageJson = maybeSourceUri.fold(maybeForkPackageJson) { sourceUri =>
          maybeForkPackageJson.as[JsObject] ++ Json.obj("repository" -> Json.obj("url" -> sourceUri))
        }

        maybeSourceOverridePackageJson.validate[PackageInfo](NPM.jsonReads) match {

          case JsSuccess(initialInfo, _) =>

            val dependenciesSansOptionals = initialInfo.dependencies.view.filterKeys(!initialInfo.optionalDependencies.contains(_)).toMap

            val infoWithResolvedOptionalDependencies = initialInfo.copy(dependencies = dependenciesSansOptionals)

            infoWithResolvedOptionalDependencies.maybeGitHubUrl.fold(Future.successful(infoWithResolvedOptionalDependencies)) { gitHubUrl =>
                gitHub.currentUrls(gitHubUrl).map {
                  case (homepage, sourceConnectionUri, issuesUrl) =>
                    infoWithResolvedOptionalDependencies.copy(
                      maybeHomepageUrl = Some(homepage),
                      sourceConnectionUri = sourceConnectionUri,
                      maybeIssuesUrl = Some(issuesUrl)
                    )
                } recover {
                  // todo: fugly
                  case error: ServerError if error.status == Status.NOT_FOUND && maybeSourceUri.isDefined =>
                    infoWithResolvedOptionalDependencies.copy(
                      maybeHomepageUrl = None,
                      sourceConnectionUri = maybeSourceUri.get,
                      maybeIssuesUrl = None
                    )
                }
            }

          case JsError(errors) =>
            Future.failed[PackageInfo](MissingMetadataException(maybeSourceOverridePackageJson, errors))
        }
      }
    }

    if (git.isGit(packageNameOrGitRepo)) {
      git.file(packageNameOrGitRepo, version, "package.json").flatMap { packageJsonString =>
        packageInfo(Json.parse(packageJsonString))
      }
    }
    else {
      if (isScoped(packageNameOrGitRepo)) {
        // can no longer get info on specific versions of scoped packages
        // so get the info for all the versions and then get the specific version out of the full list
        ws.url(registryMetadataUrl(packageNameOrGitRepo)).get().flatMap { response =>
          response.status match {
            case Status.OK =>
              val versionInfoLookup = response.json \ "versions" \ version
              versionInfoLookup.toOption.fold(Future.failed[PackageInfo](new Exception(s"Could not parse: ${response.body}")))(packageInfo)
            case _ =>
              Future.failed(new Exception(response.body))
          }
        }
      }
      else {
        ws.url(registryMetadataUrl(packageNameOrGitRepo, Some(version))).get().flatMap { response =>
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

  override def archive(packageNameOrGitRepo: String, version: Version): Future[InputStream] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.tar(packageNameOrGitRepo, version, Set("node_modules"))
    }
    else {
      Future.fromTry {
        Try {
          val url = new URL(registryTgzUrl(packageNameOrGitRepo, version))
          val inputStream = url.openConnection().getInputStream
          val gzipInputStream = new GZIPInputStream(inputStream)
          gzipInputStream
        } recoverWith {
          case _: ZipException =>
            Try {
              val url = new URL(registryTgzUrl(packageNameOrGitRepo, version))
              url.openConnection().getInputStream
            }
        }
      }
    }
  }

  override def file(packageNameOrGitRepo: String, version: Version, filename: String): Future[String] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.file(packageNameOrGitRepo, version, filename)
    }
    else {
      archiveFile(packageNameOrGitRepo, version, "package/" + filename)
    }
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    maven.convertNpmBowerDependenciesToMaven(dependencies).map { mavenDependencies =>
      mavenDependencies.map {
        case (artifactId, version) =>
          ("org.webjars.npm", artifactId, version)
      }.toSet
    }
  }

  def justDeps(nameOrUrlish: NameOrUrlish, version: Version): Future[Map[String, String]] = {
    ws.url(registryMetadataUrl(nameOrUrlish, Some(version))).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful {
            (response.json \ "dependencies").asOpt[Map[String, String]].getOrElse(Map.empty)
          }
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

  def latestDep(nameOrUrlish: NameOrUrlish, version: Version)(implicit ec: ExecutionContext): Future[Version] = {
    versions(nameOrUrlish).flatMap { availableVersions =>
      semVer.validRange(version).flatMap { maybeRange =>
        maybeRange.fold(Future.failed[Version](new Exception(s"Could not convert $version to range"))) { range =>
          semVer.maxSatisfying(availableVersions, range).flatMap { maybeVersion =>
            maybeVersion.fold(Future.failed[Version](new Exception(s"Could not find a satisfying version in range $range"))) { version =>
              Future.successful(version)
            }
          }
        }
      }
    }
  }

  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty[String, String])(implicit ec: ExecutionContext, futures: Futures): Future[Map[String, String]] = {
    import play.api.libs.concurrent.Futures._

    import scala.concurrent.duration._

    def depResolver(unresolvedDeps: Map[String, String], resolvedDeps: Map[String, String]): Future[(Map[String, String], Map[String, String])] = {
      val packagesToResolve = unresolvedDeps.view.filterKeys(!resolvedDeps.contains(_)).toMap

      val resolveFuture = Future.sequence {
        packagesToResolve.map { dep =>
          val (nameOrUrlish, versionish) = parseDep(dep)
          latestDep(nameOrUrlish, versionish).flatMap { version =>
            val newResolvedDeps = resolvedDeps + (nameOrUrlish -> version)
            justDeps(nameOrUrlish, version).map { newUnresolvedDeps =>
              (newUnresolvedDeps, newResolvedDeps)
            }
          }
        }
      }

      resolveFuture.map { resolutions =>
        if (resolutions.isEmpty) {
          (Map.empty[String, String], Map.empty[String, String])
        }
        else {
          resolutions.reduce { (acc, these) =>
            (acc._1 ++ these._1, acc._2 ++ these._2)
          }
        }
      }.flatMap { case (unresolvedDeps, resolvedDeps) =>
        if (unresolvedDeps.isEmpty) {
          Future.successful(unresolvedDeps -> resolvedDeps)
        }
        else {
          depResolver(unresolvedDeps, resolvedDeps)
        }
      }
    }

    depResolver(packageInfo.dependencies.map(parseDep), Map.empty[String, String]).map(_._2).withTimeout(10.minutes)
  }

}

object NPM {

  def uriIsh(repository: String): String = {
    if (repository.startsWith("git+https://")) {
      repository.stripPrefix("git+")
    }
    else if (repository.contains("://")) {
      // ssh://host.xz/another/repo.git
      // git://host.xz/another/repo.git
      // https://host.xz/another/repo.git
      repository
    }
    else if (repository.startsWith("gist:")) {
      // gist:11081aaa281
      repository.replace("gist:", "https://gist.github.com/") + ".git"
    }
    else if (repository.startsWith("bitbucket:")) {
      // bitbucket:example/repo
      repository.replace("bitbucket:", "https://bitbucket.org/") + ".git"
    }
    else if (repository.startsWith("gitlab:")) {
      // gitlab:another/repo
      repository.replace("gitlab:", "https://gitlab.com/") + ".git"
    }
    else if (repository.contains(":/")) {
      // host.xz:/another/repo.git
      // user@host.xz:/another/repo.git
      "ssh://" + repository
    }
    else if (repository.contains(":")) {
      // host.xz:another/repo.git
      "ssh://" + repository.replace(":", "/")
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

  val homepageReader: Reads[Option[URL]] = {
    (__ \ "homepage").readNullable[URL]
  }

  val homepageToIssuesReader: Reads[URL] = {

    def issuesUrl(url: URL): Option[URL] = GitHub.gitHubIssuesUrl(url).orElse(Bitbucket.bitbucketIssuesUrl(url)).toOption

    val error = JsonValidationError("Could not figure out the issues URL.")

    homepageReader.collect(error) {
      // todo: nasty
      case Some(url) if issuesUrl(url).isDefined => issuesUrl(url).get
    }
  }

  val bugsReaderNullable: Reads[Option[URL]] = {
    Reads.optionNoError {
      (__ \ "bugs").read[URL]
        .orElse((__ \ "bugs" \ "url").read[URL])
        .orElse(homepageToIssuesReader)
    }
  }

  implicit val jsonReads: Reads[PackageInfo] = {
    val repositoryUrlReader: Reads[String] = (__ \ "repository").read[String].orElse((__ \ "repository" \ "url").read[String])

    val sourceConnectionUriReader: Reads[URI] = repositoryUrlReader.map(repositoryUrlToJsString).andThen(PackageInfo.readsUri)

    val licenseReader = (__ \ "license").read[Seq[String]]
      .orElse((__ \ "license").read[String].map(Seq(_)))
      .orElse((__ \ "license").read[JsObject].map(_.\("type").as[String]).map(Seq(_)))
      .orElse((__ \ "licenses").read[Seq[JsObject]].map(_.map(_.\("type").as[String])))
      .orElse((__ \ "licenses").read[String].map(Seq(_)))
      .orElse(Reads.pure(Seq.empty[String]))

    val nameReader = (__ \ "name").read[String]

    (
      nameReader ~
      (__ \ "version").read[String] ~
      homepageReader ~
      sourceConnectionUriReader ~
      bugsReaderNullable ~
      licenseReader ~
      (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
      (__ \ "optionalDependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
      Reads.pure(None)
    )(PackageInfo.apply _)
  }

}
