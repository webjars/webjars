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
        // 404 → the artifact simply isn't tracked in webjars-classic (a true
        // "doesn't support this" answer). Other errors (rate-limit, 5xx)
        // propagate so the caller can distinguish transient failures.
        .catchSome:
          case ServerError(_, 404) => ZIO.fail(new Exception(s"The Classic WebJar $nameOrUrlish does not support this"))
        .run
      ZIO.fromTry(Classic.parseMetadata(nameOrUrlish, propertiesString)).run

  override def artifactId(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, MavenCentral.ArtifactId] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.map(_.id).orElseSucceed(MavenCentral.ArtifactId(nameOrUrlish))

  override def excludes(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Set[String]] = ZIO.succeed(Set.empty)

  override def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Option[String]] =
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(maybeBaseDirGlobFromMetadata)

  def license(metadata: Metadata): ZIO[Scope, Throwable, LicenseMetadata] =
    metadata match
      case metadataNormal: MetadataNormal =>
        Classic.providedLicense(metadataNormal.licenseName, metadataNormal.licenseUrl).getOrElse {
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
                val maybeSpdxId = json.asObject
                  .flatMap(_.get("license"))
                  .flatMap(_.asObject)
                  .flatMap(_.get("spdx_id"))
                  .flatMap(_.asString)
                // GitHub returns spdx_id="NOASSERTION" when its license-detection
                // can't classify the LICENSE file (e.g. a custom variant). Treat
                // that — along with a missing/empty spdx_id — as an unresolved
                // license so callers can fall back to the LICENSE-file detector
                // or to the .properties override.
                maybeSpdxId match
                  case Some(spdxId) if spdxId.nonEmpty && spdxId != "NOASSERTION" =>
                    LicenseMetadata.SpdxLicense(spdxId)
                  case _ =>
                    LicenseMetadata.UnresolvedLicense
              case _ =>
                val body = response.body.asString.run
                ZIO.fail(ServerError(body, response.status.code)).run
        }

      case metadataNpm: MetadataNpm =>
        Classic.providedLicense(metadataNpm.licenseName, metadataNpm.licenseUrl)
          .getOrElse(ZIO.succeed(LicenseMetadata.UnresolvedLicense))

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
          // A `repo=owner/repo` override in the .properties takes precedence
          // over the caller's `maybeSourceUri` and over whatever `repository`
          // the upstream package.json declares (often missing for scoped
          // packages whose NPM org name doesn't match the GitHub org name —
          // e.g. @tabby_ai vs github.com/tabby-ai).
          val effectiveSourceUri = metadataNpm.repo
            .flatMap(repo => URL.parseOption(s"https://github.com/$repo"))
            .orElse(maybeSourceUri)
          val packageInfo = npm.info(metadataNpm.packageName, version, effectiveSourceUri).run
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
      case metadataNpm: MetadataNpm =>
        // Honor a `base.dir` override (e.g. `*/dist`) before falling back to
        // the NPM default (`*/`, which keeps the whole tarball minus the
        // top-level `package/` directory).
        metadataNpm.baseDir.fold(npm.maybeBaseDirGlob(metadataNpm.packageName))(d => ZIO.succeed(Some(d)))

  def licensesFromMetadata(metadata: Metadata, version: Version, packageInfo: PackageInfo): ZIO[Scope, Throwable, Set[License]] =
    licenses(metadata.id.toString, version, packageInfo)

object Classic:

  val live: ZLayer[Client & LicenseDetector & GitHub & Cache & AppConfig & NPM, Nothing, Classic] = ZLayer.derive[ClassicLive]

  sealed trait Metadata:
    val id: MavenCentral.ArtifactId

  case class MetadataNormal(id: MavenCentral.ArtifactId, name: String, repo: String, download: Option[String], requireJsMain: Option[String], baseDir: Option[String], licenseName: Option[String], licenseUrl: Option[String]) extends Metadata

  case class MetadataNpm(id: MavenCentral.ArtifactId, packageName: String, repo: Option[String], baseDir: Option[String], licenseName: Option[String], licenseUrl: Option[String]) extends Metadata

  /** Lift the optional `license.name` / `license.url` overrides from a
   *  properties file into a `LicenseMetadata`. Returns `None` when neither
   *  override is set so callers can fall back to their own detection path. */
  private[utils] def providedLicense(maybeName: Option[String], maybeUrl: Option[String]): Option[ZIO[Any, Throwable, LicenseMetadata]] =
    (maybeName, maybeUrl) match
      case (Some(licenseName), Some(licenseUrl)) =>
        Some(ZIO.fromTry {
          URL.parseTry(licenseUrl).map { absoluteLicenseUrl =>
            LicenseMetadata.ProvidedLicense(LicenseWithNameAndUrl(licenseName, absoluteLicenseUrl))
          }
        })
      case (Some(licenseName), None) =>
        Some(ZIO.succeed(LicenseMetadata.ProvidedLicense(LicenseWithName(licenseName))))
      case (None, Some(licenseUrl)) =>
        Some(ZIO.fromTry {
          URL.parseTry(licenseUrl).map { absoluteLicenseUrl =>
            LicenseMetadata.ProvidedLicense(LicenseWithUrl(absoluteLicenseUrl))
          }
        })
      case (None, None) =>
        None

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
        yield MetadataNormal(MavenCentral.ArtifactId(nameOrUrlish), name, repo, maybeDownload, maybeRequireJsMain, maybeBaseDir, maybeLicenseName, maybeLicenseUrl)
      } { npmName =>
        Some(MetadataNpm(MavenCentral.ArtifactId(nameOrUrlish), npmName, maybeRepo, maybeBaseDir, maybeLicenseName, maybeLicenseUrl))
      }
    }.flatMap {
      case Some(metadata) => scala.util.Success(metadata)
      case None => scala.util.Failure(new Exception("Invalid properties file: missing required fields (name and repo for normal, or npm for npm-based)"))
    }
