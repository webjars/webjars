package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import io.lemonlabs.uri.{AbsoluteUrl, Url}
import io.lemonlabs.uri.typesafe.dsl.urlToUrlDsl
import webjars.utils.Deployable.{NameOrUrlish, Version}
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.io.InputStream
import java.util.zip.{GZIPInputStream, ZipException}
import scala.util.Try

trait NPM extends Deployable:
  override val name: String = "NPM"
  override val groupId: MavenCentral.GroupId = MavenCentral.GroupId("org.webjars.npm")
  override val metadataFile: Option[String] = Some("package.json")
  def versionJson(packageNameOrGitRepo: NameOrUrlish, version: Version): ZIO[Scope, Throwable, Json]
  def justDeps(nameOrUrlish: NameOrUrlish, version: Version): ZIO[Scope, Throwable, Map[String, String]]
  def latestDep(nameOrUrlish: NameOrUrlish, version: Version): ZIO[Scope, Throwable, Version]

case class NPMLive(client: Client, licenseDetector: LicenseDetector, git: Git, gitHub: GitHub, maven: Maven, semVer: SemVer) extends NPM:

  private val BASE_URL: AbsoluteUrl = AbsoluteUrl.parse("https://registry.npmjs.org")

  override def artifactId(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, MavenCentral.ArtifactId] =
    git.artifactId(nameOrUrlish).map(MavenCentral.ArtifactId(_))

  override def excludes(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Set[String]] =
    ZIO.succeed(Set("node_modules"))

  override def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Option[Version]] =
    ZIO.succeed(Some("*/"))

  private def registryMetadataUrl(packageName: String, maybeVersion: Option[Version] = None): Url =
    maybeVersion.fold {
      BASE_URL / packageName
    } { version =>
      BASE_URL / packageName / version
    }

  private def isScoped(maybeScopeAndPackageName: String): Boolean =
    maybeScopeAndPackageName.contains('/') && maybeScopeAndPackageName.startsWith("@")

  private def registryTgzUrl(maybeScopeAndPackageName: String, version: Version): Url =
    if isScoped(maybeScopeAndPackageName) then
      val parts = maybeScopeAndPackageName.split('/')
      val scope = parts.head
      val packageName = parts.last
      BASE_URL / scope / packageName / "-" / s"$packageName-$version.tgz"
    else
      BASE_URL / maybeScopeAndPackageName / "-" / s"$maybeScopeAndPackageName-$version.tgz"

  override def versions(packageNameOrGitRepo: NameOrUrlish): ZIO[Scope, Throwable, Set[Version]] =
    if git.isGit(packageNameOrGitRepo) then
      git.versions(packageNameOrGitRepo)
    else
      defer:
        val url = registryMetadataUrl(packageNameOrGitRepo).toString()
        val request = Request.get(URL.decode(url).toOption.get)
          .addHeader(Header.Accept(MediaType("application", "vnd.npm.install-v1+json")))
        val response = client.request(request).run
        response.status match
          case Status.Ok =>
            val body = response.body.asString.run
            val json = ZIO.fromEither(body.fromJson[Json].left.map(new Exception(_))).run
            json.asObject.flatMap(_.get("versions")).flatMap(_.asObject).map(_.keys.toSet)
              .getOrElse(Set.empty)
          case _ =>
            val body = response.body.asString.run
            ZIO.fail(new Exception(body)).run

  def versionJson(packageNameOrGitRepo: NameOrUrlish, version: Version): ZIO[Scope, Throwable, Json] =
    defer:
      val url = registryMetadataUrl(packageNameOrGitRepo, Some(version))
      val response = client.request(Request.get(URL.decode(url.toString()).toOption.get)).run
      response.status match
        case Status.Ok =>
          val body = response.body.asString.run
          ZIO.fromEither(body.fromJson[Json].left.map(new Exception(_))).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(body)).run

  private def jsonString(json: Json, field: String): Option[String] =
    json.asObject.flatMap(_.get(field)).flatMap(_.asString)

  private def jsonObject(json: Json, field: String): Option[Json] =
    json.asObject.flatMap(_.get(field))

  private def jsonMapStringString(json: Json, field: String): Map[String, String] =
    jsonObject(json, field).flatMap(_.asObject).map { obj =>
      obj.toMap.collect { case (k, v) if v.asString.isDefined => k -> v.asString.get }
    }.getOrElse(Map.empty)

  private def parsePackageInfo(json: Json): ZIO[Scope, Throwable, PackageInfo] =
    val name = jsonString(json, "name").getOrElse("")
    val version = jsonString(json, "version").getOrElse("")

    val maybeHomepageUrl = jsonString(json, "homepage").flatMap(AbsoluteUrl.parseOption)

    val repositoryUrl = jsonObject(json, "repository").flatMap { repo =>
      repo.asString.orElse(jsonString(repo, "url"))
    }.getOrElse("")

    val sourceConnectionUriString = NPM.uriIsh(repositoryUrl)

    val sourceConnectionUri = AbsoluteUrl.parseTry(sourceConnectionUriString) match
      case scala.util.Success(url) => url
      case scala.util.Failure(_) => AbsoluteUrl.parse(s"https://github.com/$name")

    val maybeIssuesUrl =
      jsonString(json, "bugs").flatMap(AbsoluteUrl.parseOption)
        .orElse(jsonObject(json, "bugs").flatMap(b => jsonString(b, "url")).flatMap(AbsoluteUrl.parseOption))
        .orElse(maybeHomepageUrl.flatMap(u => GitHub.gitHubIssuesUrl(u).toOption.orElse(Bitbucket.bitbucketIssuesUrl(u).toOption)))

    val licenseSeq: Seq[String] = {
      val licenseField = jsonObject(json, "license")
      val licensesField = jsonObject(json, "licenses")
      licenseField.map { lic =>
        lic.asString.toSeq ++
          lic.asArray.toSeq.flatMap(_.flatMap(_.asString)) ++
          lic.asObject.flatMap(o => jsonString(lic, "type")).toSeq
      }.orElse {
        licensesField.map { lics =>
          lics.asArray.toSeq.flatMap(_.flatMap(l => jsonString(l, "type"))) ++
            lics.asString.toSeq
        }
      }.getOrElse(Seq.empty)
    }

    val metadataLicenses = licenseSeq.map(LicenseMetadata.SpdxLicense(_))
    val dependencies = jsonMapStringString(json, "dependencies")
    val optionalDependencies = jsonMapStringString(json, "optionalDependencies")

    ZIO.succeed(PackageInfo(
      name = name,
      version = version,
      maybeHomepageUrl = maybeHomepageUrl,
      sourceConnectionUri = sourceConnectionUri,
      maybeIssuesUrl = maybeIssuesUrl,
      metadataLicenses = metadataLicenses,
      dependencies = dependencies,
      optionalDependencies = optionalDependencies,
      maybeTag = None,
    ))

  override def info(packageNameOrGitRepo: NameOrUrlish, version: Version, maybeSourceUri: Option[AbsoluteUrl] = None): ZIO[Scope, Throwable, PackageInfo] =

    def resolvePackageJson(packageJson: Json): ZIO[Scope, Throwable, Json] =
      defer:
        val withForkRepo = if git.isGit(packageNameOrGitRepo) then
          val gitUrlStr = git.gitUrl(packageNameOrGitRepo).run
          val repoObj = Json.Obj("url" -> Json.Str(gitUrlStr))
          packageJson.asObject.map(obj => Json.Obj(obj.toMap.updated("repository", repoObj).toSeq*)).getOrElse(packageJson)
        else
          packageJson

        maybeSourceUri.fold(withForkRepo) { sourceUri =>
          val repoObj = Json.Obj("url" -> Json.Str(sourceUri.toString))
          withForkRepo.asObject.map(obj => Json.Obj(obj.toMap.updated("repository", repoObj).toSeq*)).getOrElse(withForkRepo)
        }

    def resolveGitHubUrls(info: PackageInfo): ZIO[Scope, Throwable, PackageInfo] =
      info.maybeGitHubUrl.fold(ZIO.succeed(info)) { gitHubUrl =>
        gitHub.currentUrls(gitHubUrl).map { case (homepage, sourceConnectionUri, issuesUrl) =>
          info.copy(
            maybeHomepageUrl = Some(homepage),
            sourceConnectionUri = sourceConnectionUri,
            maybeIssuesUrl = Some(issuesUrl)
          )
        }.catchSome {
          case error: ServerError if error.status == 404 && maybeSourceUri.isDefined =>
            ZIO.succeed(info.copy(
              maybeHomepageUrl = None,
              sourceConnectionUri = maybeSourceUri.get,
              maybeIssuesUrl = None
            ))
        }
      }

    def packageInfo(packageJson: Json): ZIO[Scope, Throwable, PackageInfo] =
      defer:
        val resolvedJson = resolvePackageJson(packageJson).run
        val initialInfo = parsePackageInfo(resolvedJson).run
        val dependenciesSansOptionals = initialInfo.dependencies.view.filterKeys(!initialInfo.optionalDependencies.contains(_)).toMap
        val infoWithResolvedOptionalDependencies = initialInfo.copy(dependencies = dependenciesSansOptionals)
        resolveGitHubUrls(infoWithResolvedOptionalDependencies).run

    defer:
      val json = if git.isGit(packageNameOrGitRepo) then
        val packageJsonString = file(packageNameOrGitRepo, version, "package.json").run
        ZIO.fromEither(packageJsonString.fromJson[Json].left.map(new Exception(_))).run
      else
        versionJson(packageNameOrGitRepo, version).run
      packageInfo(json).run

  override def archive(packageNameOrGitRepo: String, version: Version): ZIO[Scope, Throwable, InputStream] =
    if git.isGit(packageNameOrGitRepo) then
      git.tar(packageNameOrGitRepo, version, Set("node_modules"))
    else
      ZIO.fromTry {
        Try {
          val url = registryTgzUrl(packageNameOrGitRepo, version).toJavaURI.toURL
          val inputStream = url.openConnection().getInputStream
          val gzipInputStream = new GZIPInputStream(inputStream)
          gzipInputStream
        }.recoverWith {
          case _: ZipException =>
            Try {
              val url = registryTgzUrl(packageNameOrGitRepo, version).toJavaURI.toURL
              url.openConnection().getInputStream
            }
        }
      }

  override def file(packageNameOrGitRepo: String, version: Version, filename: String): ZIO[Scope, Throwable, String] =
    if git.isGit(packageNameOrGitRepo) then
      defer:
        val url = ZIO.fromTry(GitHub.gitHubUrl(packageNameOrGitRepo)).run
        gitHub.raw(url, version, filename).run
      .catchAll(_ => git.file(packageNameOrGitRepo, version, filename))
    else
      archiveFile(packageNameOrGitRepo, version, "package/" + filename)

  override def mavenDependencies(dependencies: Map[String, String]): ZIO[Scope, Throwable, Set[(MavenCentral.GroupArtifact, String)]] =
    maven.convertNpmDependenciesToMaven(dependencies).map { mavenDependencies =>
      mavenDependencies.map { case (artifactId, version) =>
        MavenCentral.GroupArtifact(groupId, MavenCentral.ArtifactId(artifactId)) -> version
      }.toSet
    }

  def justDeps(nameOrUrlish: NameOrUrlish, version: Version): ZIO[Scope, Throwable, Map[String, String]] =
    versionJson(nameOrUrlish, version).map { json =>
      jsonMapStringString(json, "dependencies")
    }

  def latestDep(nameOrUrlish: NameOrUrlish, version: Version): ZIO[Scope, Throwable, Version] =
    defer:
      val maybeRange = semVer.validRange(version).run
      val range = ZIO.fromOption(maybeRange)
        .orElseFail(new Exception(s"For $nameOrUrlish could not convert $version to range"))
        .run
      if (range == version) && (!range.startsWith(">")) && (!range.startsWith("=")) && (!range.startsWith("<")) && (range != "*") then
        version
      else
        val availableVersions = versions(nameOrUrlish).run
        val maybeMax = semVer.maxSatisfying(availableVersions, range).run
        ZIO.fromOption(maybeMax)
          .orElseFail(new Exception(s"For $nameOrUrlish could not find a satisfying version in range $range"))
          .run

  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty[String, String]): ZIO[Scope, Throwable, Map[String, String]] =

    def depResolver(unresolvedDeps: Map[String, String], resolvedDeps: Map[String, String]): ZIO[Scope, Throwable, (Map[String, String], Map[String, String])] =
      val packagesToResolve = unresolvedDeps.view.filterKeys(!resolvedDeps.contains(_)).toMap

      defer:
        val resolutions = ZIO.foreach(packagesToResolve.toSeq) { dep =>
          defer:
            val (nameOrUrlish, versionish) = parseDep(dep)
            val version = latestDep(nameOrUrlish, versionish).run
            val newResolvedDeps = resolvedDeps + (nameOrUrlish -> version)
            val newUnresolvedDeps = justDeps(nameOrUrlish, version).run
            (newUnresolvedDeps, newResolvedDeps)
        }.run

        val (newUnresolved, newResolved) =
          if resolutions.isEmpty then
            (Map.empty[String, String], resolvedDeps)
          else
            resolutions.reduce((acc, these) => (acc._1 ++ these._1, acc._2 ++ these._2))

        if newUnresolved.isEmpty then
          newUnresolved -> newResolved
        else
          depResolver(newUnresolved, newResolved).run

    depResolver(packageInfo.dependencies.map(parseDep), Map.empty[String, String]).map(_._2).timeoutFail(new Exception("Dependency graph resolution timed out"))(10.minutes)

object NPM:

  val live: ZLayer[Client & LicenseDetector & Git & GitHub & Maven & SemVer, Nothing, NPM] = ZLayer.derive[NPMLive]

  def uriIsh(repository: String): String =
    if repository.startsWith("git+https://") then
      repository.stripPrefix("git+")
    else if repository.contains("://") then
      repository
    else if repository.startsWith("gist:") then
      repository.replace("gist:", "https://gist.github.com/") + ".git"
    else if repository.startsWith("bitbucket:") then
      repository.replace("bitbucket:", "https://bitbucket.org/") + ".git"
    else if repository.startsWith("github:") then
      repository.replace("github:", "https://github.com/") + ".git"
    else if repository.startsWith("gitlab:") then
      repository.replace("gitlab:", "https://gitlab.com/") + ".git"
    else if repository.contains(":/") then
      "ssh://" + repository.replace(":/", "/")
    else if repository.contains(":") then
      "ssh://" + repository.replace(":", "/")
    else if repository.contains("/") then
      "https://github.com/" + repository + ".git"
    else
      repository

  def repositoryToUri(repositoryUrl: String): Option[AbsoluteUrl] =
    AbsoluteUrl.parseOption(uriIsh(repositoryUrl))
