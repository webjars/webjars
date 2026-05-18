package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.utils.Deployable.{NameOrUrlish, Version}
import zio.*
import zio.direct.*
import zio.http.*

import java.io.{InputStream, StringReader}
import java.util.Properties
import java.util.zip.GZIPInputStream
import scala.util.Try

trait Classic extends Deployable:
  import Classic.*
  override val name: String = "Classic"
  override val groupId: MavenCentral.GroupId = MavenCentral.GroupId("org.webjars")
  override val metadataFile: Option[String] = None
  def metadata(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Metadata]
  def license(metadata: Metadata): ZIO[Scope, Throwable, LicenseMetadata]
  def infoFromMetadata(metadata: Metadata, version: Version, maybeSourceUri: Option[URL]): ZIO[Scope, Throwable, PackageInfo]
  def archiveFromMetadata(metadata: Metadata, version: Version): ZIO[Scope, Throwable, InputStream]
  def maybeBaseDirGlobFromMetadata(metadata: Metadata): ZIO[Scope, Throwable, Option[String]]
  def licensesFromMetadata(metadata: Metadata, version: Version, packageInfo: PackageInfo): ZIO[Scope, Throwable, Set[License]]

case class ClassicLive(httpClient: Client, licenseDetector: LicenseDetector, gitHub: GitHub, cache: Cache, config: AppConfig, npm: NPM) extends Classic:
  import Classic.*

  private lazy val webJarsClassicBranch = "main"

  def metadata(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Metadata] =
    defer:
      val propertiesString = gitHub.raw(URL.unsafeParse("https://github.com/webjars/webjars-classic"), webJarsClassicBranch, s"$nameOrUrlish.properties")
        .catchAll(_ => ZIO.fail(new Exception(s"The Classic WebJar $nameOrUrlish does not support this")))
        .run
      ZIO.fromTry(Classic.parseMetadata(nameOrUrlish, propertiesString)).run

  override def artifactId(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, MavenCentral.ArtifactId] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.map(_.id)

  override def excludes(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Set[String]] = ZIO.succeed(Set.empty)

  override def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Option[String]] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(maybeBaseDirGlobFromMetadata)

  def license(metadata: Metadata): ZIO[Scope, Throwable, LicenseMetadata] =
    metadata match
      case metadataNormal: MetadataNormal =>
        defer:
          val url = s"https://api.github.com/repos/${metadataNormal.repo}/license"
          val baseRequest = Request.get(URL.decode(url).toOption.get)
            .addHeader(Header.Accept(MediaType.application.json))
          val request = gitHub.maybeAuthToken.fold(baseRequest)(token =>
            baseRequest.addHeader(Header.Authorization.Bearer(token))
          )
          val response = httpClient.batched(request).run
          response.status match
            case Status.Ok =>
              import zio.json.*
              import zio.json.ast.Json
              val body = response.body.asString.run
              val json = ZIO.fromEither(body.fromJson[Json].left.map(new Exception(_))).run
              val spdxId = json.asObject
                .flatMap(_.get("license"))
                .flatMap(_.asObject)
                .flatMap(_.get("spdx_id"))
                .flatMap(_.asString)
                .getOrElse("Unknown")
              LicenseMetadata.SpdxLicense(spdxId)
            case _ =>
              val body = response.body.asString.run
              ZIO.fail(ServerError(body, response.status.code)).run

      case metadataNpm: MetadataNpm =>
        (metadataNpm.licenseName, metadataNpm.licenseUrl) match
          case (Some(licenseName), Some(licenseUrl)) =>
            ZIO.fromTry {
              URL.parseTry(licenseUrl).map { absoluteLicenseUrl =>
                LicenseMetadata.ProvidedLicense(LicenseWithNameAndUrl(licenseName, absoluteLicenseUrl))
              }
            }
          case (Some(licenseName), None) =>
            ZIO.succeed(LicenseMetadata.ProvidedLicense(LicenseWithName(licenseName)))
          case (None, Some(licenseUrl)) =>
            ZIO.fromTry {
              URL.parseTry(licenseUrl).map { absoluteLicenseUrl =>
                LicenseMetadata.ProvidedLicense(LicenseWithUrl(absoluteLicenseUrl))
              }
            }
          case _ =>
            ZIO.succeed(LicenseMetadata.UnresolvedLicense)

  override def info(nameOrUrlish: NameOrUrlish, version: Version, maybeSourceUri: Option[URL]): ZIO[Scope, Throwable, PackageInfo] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(infoFromMetadata(_, version, maybeSourceUri))

  override def mavenDependencies(dependencies: Map[String, String]): ZIO[Scope, Throwable, Set[(MavenCentral.GroupArtifact, String)]] =
    ZIO.succeed(Set.empty)

  private def downloadExists(url: String): ZIO[Scope, Throwable, URL] =
    defer:
      val absoluteUrl = ZIO.fromTry(URL.parseTry(url)).run
      val response = httpClient.batched(Request.head(absoluteUrl)).run
      response.status match
        case s if s.isSuccess || s.isRedirection => absoluteUrl
        case _ => ZIO.fail(new Exception(s"${absoluteUrl.encode} does not exist")).run

  private def validDownload(download: String, version: Version): ZIO[Scope, Throwable, URL] =
    downloadExists(download.replace("${version}", version)).catchAll { _ =>
      downloadExists(download.replace("${version}", version.vless))
    }

  override def archive(nameOrUrlish: NameOrUrlish, version: Version): ZIO[Scope, Throwable, InputStream] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(archiveFromMetadata(_, version))

  override def file(nameOrUrlish: NameOrUrlish, version: Version, filename: String): ZIO[Scope, Throwable, String] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap {
      case metadataNormal: MetadataNormal =>
        GitHub.gitHubUrl(s"https://github.com/${metadataNormal.repo}").fold(
          e => ZIO.fail(e),
          gitHubUrl => gitHub.raw(gitHubUrl, version, filename)
        )
      case _: MetadataNpm =>
        npm.file(nameOrUrlish, version, filename)
    }

  override def versions(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Set[Version]] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap {
      case metadataNormal: MetadataNormal =>
        gitHub.tags(metadataNormal.repo)
      case _: MetadataNpm =>
        npm.versions(nameOrUrlish)
    }

  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String]): ZIO[Scope, Throwable, Map[String, String]] =
    ZIO.succeed(Map.empty)

  def infoFromMetadata(metadata: Metadata, version: Version, maybeSourceUri: Option[URL]): ZIO[Scope, Throwable, PackageInfo] =
    metadata match
      case metadataNormal: MetadataNormal =>
        val gitHubUrl = GitHub.gitHubUrl(s"https://github.com/${metadataNormal.repo}")
        gitHubUrl.flatMap(GitHub.gitHubGitUrl).fold(
          e => ZIO.fail(e),
          gitHubGitUri =>
            license(metadataNormal).map { licenseMetadata =>
              PackageInfo(
                metadataNormal.name,
                version.vless,
                gitHubUrl.toOption,
                gitHubGitUri,
                gitHubUrl.flatMap(GitHub.gitHubIssuesUrl).toOption,
                Seq(licenseMetadata),
                Map.empty,
                Map.empty,
                Some(version),
              )
            }
        )
      case metadataNpm: MetadataNpm =>
        defer:
          val packageInfo = npm.info(metadataNpm.packageName, version, maybeSourceUri).run
          val licenseMetadata = license(metadataNpm).run
          packageInfo.copy(metadataLicenses = Seq(licenseMetadata))

  def archiveFromMetadata(metadata: Metadata, version: Version): ZIO[Scope, Throwable, InputStream] =
    metadata match
      case metadataNormal: MetadataNormal =>
        metadataNormal.download.fold {
          downloadExists(s"https://github.com/${metadataNormal.repo}/archive/$version.zip")
        } { download =>
          validDownload(download, version)
        }.map { absoluteUrl =>
          val is = absoluteUrl.toJavaURI.toURL.openStream()
          if absoluteUrl.encode.endsWith("tgz") then
            new GZIPInputStream(is)
          else
            is
        }
      case metadataNpm: MetadataNpm =>
        npm.archive(metadataNpm.packageName, version)

  def maybeBaseDirGlobFromMetadata(metadata: Metadata): ZIO[Scope, Throwable, Option[String]] =
    metadata match
      case metadataNormal: MetadataNormal => ZIO.succeed(metadataNormal.baseDir)
      case metadataNpm: MetadataNpm => npm.maybeBaseDirGlob(metadataNpm.packageName)

  def licensesFromMetadata(metadata: Metadata, version: Version, packageInfo: PackageInfo): ZIO[Scope, Throwable, Set[License]] =
    licenses(metadata.id.toString, version, packageInfo)

object Classic:

  val live: ZLayer[Client & LicenseDetector & GitHub & Cache & AppConfig & NPM, Nothing, Classic] = ZLayer.derive[ClassicLive]

  sealed trait Metadata:
    val id: MavenCentral.ArtifactId

  case class MetadataNormal(id: MavenCentral.ArtifactId, name: String, repo: String, download: Option[String], requireJsMain: Option[String], baseDir: Option[String]) extends Metadata

  case class MetadataNpm(id: MavenCentral.ArtifactId, packageName: String, licenseName: Option[String], licenseUrl: Option[String]) extends Metadata

  def parseMetadata(nameOrUrlish: String, propertiesString: String): Try[Metadata] =
    Try {
      val properties = new Properties()
      properties.load(new StringReader(propertiesString))

      val maybeName = Option(properties.getProperty("name"))
      val maybeRepo = Option(properties.getProperty("repo"))
      val maybeDownload = Option(properties.getProperty("download"))
      val maybeRequireJsMain = Option(properties.getProperty("requirejs.main"))
      val maybeBaseDir = Option(properties.getProperty("base.dir"))
      val maybeNpm = Option(properties.getProperty("npm"))
      val maybeLicenseName = Option(properties.getProperty("license.name"))
      val maybeLicenseUrl = Option(properties.getProperty("license.url"))

      maybeNpm.fold[Option[Metadata]] {
        for
          name <- maybeName
          repo <- maybeRepo
        yield MetadataNormal(MavenCentral.ArtifactId(nameOrUrlish), name, repo, maybeDownload, maybeRequireJsMain, maybeBaseDir)
      } { npmName =>
        Some(MetadataNpm(MavenCentral.ArtifactId(nameOrUrlish), npmName, maybeLicenseName, maybeLicenseUrl))
      }
    }.flatMap {
      case Some(metadata) => scala.util.Success(metadata)
      case None => scala.util.Failure(new Exception("Invalid properties file: missing required fields (name and repo for normal, or npm for npm-based)"))
    }
