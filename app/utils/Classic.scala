package utils

import com.jamesward.zio_mavencentral.MavenCentral
import io.lemonlabs.uri.AbsoluteUrl
import io.lemonlabs.uri.typesafe.dsl.{pathPartToUrlDsl, urlToUrlDsl}
import play.api.Configuration
import play.api.http.Status
import play.api.i18n.{Langs, MessagesApi}
import play.api.libs.concurrent.Futures
import play.api.libs.ws.WSClient
import utils.Deployable.{NameOrUrlish, Version}

import java.io.{InputStream, StringReader}
import java.util.Properties
import java.util.zip.GZIPInputStream
import javax.inject.Inject
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Classic @Inject() (ws: WSClient, val licenseDetector: LicenseDetector, val messages: MessagesApi, val langs: Langs, gitHub: GitHub, cache: Cache, configuration: Configuration, npm: NPM)(using ec: ExecutionContext) extends Deployable {
  import Classic.*

  override val name: String = "Classic"
  override val groupId: MavenCentral.GroupId = MavenCentral.GroupId("org.webjars")

  private lazy val webJarsClassicBranch = configuration.getOptional[String]("webjars.classic.branch").getOrElse("main")

  def metadata(nameOrUrlish: NameOrUrlish): Future[Metadata] = {
    gitHub.raw(AbsoluteUrl.parse("https://github.com/webjars/webjars-classic"), webJarsClassicBranch, s"$nameOrUrlish.properties")
      .recoverWith {
        case _ =>
          Future.failed(
            new Exception(s"The Classic WebJar $nameOrUrlish does not support this")
          )
      }
      .flatMap { propertiesString =>
        Future.fromTry(Classic.parseMetadata(nameOrUrlish, propertiesString))
      }
  }

  override def artifactId(nameOrUrlish: NameOrUrlish): Future[MavenCentral.ArtifactId] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.map(_.id)
  }

  override def excludes(nameOrUrlish: NameOrUrlish): Future[Set[String]] = Future.successful(Set.empty)

  override val metadataFile: Option[String] = None
  override def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): Future[Option[String]] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(maybeBaseDirGlobFromMetadata)
  }

  def license(metadata: Metadata): Future[LicenseMetadata] = {
    metadata match {
      case metadataNormal: MetadataNormal =>
        val req = gitHub.maybeAuthToken.fold(
          ws.url(s"https://api.github.com/repos/${metadataNormal.repo}/license")
        )( accessToken =>
          gitHub.ws(s"repos/${metadataNormal.repo}/license", accessToken)
        )
        req.get().flatMap { response =>
          response.status match {
            case Status.OK =>
              Future.successful(
                LicenseMetadata.SpdxLicense((response.json \ "license" \ "spdx_id").as[String])
              )
            case _ =>
              Future.failed(ServerError(response.body, response.status))
          }
        }

      case metadataNpm: MetadataNpm =>
        (metadataNpm.licenseName, metadataNpm.licenseUrl) match {
          case (Some(licenseName), Some(licenseUrl)) =>
            Future.fromTry {
              AbsoluteUrl.parseTry(licenseUrl).map { absoluteLicenseUrl =>
                LicenseMetadata.ProvidedLicense(LicenseWithNameAndUrl(licenseName, absoluteLicenseUrl))
              }
            }
          case (Some(licenseName), None) =>
            Future.successful(LicenseMetadata.ProvidedLicense(LicenseWithName(licenseName)))
          case (None, Some(licenseUrl)) =>
            Future.fromTry {
              AbsoluteUrl.parseTry(licenseUrl).map { absoluteLicenseUrl =>
                LicenseMetadata.ProvidedLicense(LicenseWithUrl(absoluteLicenseUrl))
              }
            }
          case _ =>
            Future.successful(LicenseMetadata.UnresolvedLicense)
        }
    }
  }

  override def info(nameOrUrlish: NameOrUrlish, version: Version, maybeSourceUri: Option[AbsoluteUrl]): Future[PackageInfo] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(infoFromMetadata(_, version, maybeSourceUri))
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(MavenCentral.GroupArtifact, String)]] =
    Future.successful(Set.empty)

  private def downloadExists(url: String): Future[AbsoluteUrl] = {
    Future.fromTry {
      AbsoluteUrl.parseTry(url)
    }.flatMap { absoluteUrl =>
      ws.url(url).head().flatMap { response =>
        response.status match {
          case Status.OK => Future.successful(absoluteUrl)
          case _ => Future.failed(new Exception(s"$absoluteUrl does not exist"))
        }
      }
    }
  }

  private def validDownload(download: String, version: Version): Future[AbsoluteUrl] = {
    downloadExists(download.replace("${version}", version)).fallbackTo {
      downloadExists(download.replace("${version}", version.vless))
    }
  }

  override def archive(nameOrUrlish: NameOrUrlish, version: Version): Future[InputStream] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap(archiveFromMetadata(_, version))
  }

  override def file(nameOrUrlish: NameOrUrlish, version: Version, filename: String): Future[String] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap {
      case metadataNormal: MetadataNormal =>
        GitHub.gitHubUrl(s"https://github.com/${metadataNormal.repo}").fold(Future.failed[String], { gitHubUrl =>
          gitHub.raw(gitHubUrl, version, filename)
        })
      case _: MetadataNpm =>
        npm.file(nameOrUrlish, version, filename)
    }
  }

  override def versions(nameOrUrlish: NameOrUrlish): Future[Set[Version]] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap {
      case metadataNormal: MetadataNormal =>
        gitHub.tags(metadataNormal.repo)
      case _: MetadataNpm =>
        npm.versions(nameOrUrlish)
    }
  }

  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String])(using ec: ExecutionContext, futures: Futures): Future[Map[String, String]] = {
    Future.successful(Map.empty)
  }

  // Methods that work with pre-provided metadata (for createClassic endpoint)

  def infoFromMetadata(metadata: Metadata, version: Version, maybeSourceUri: Option[AbsoluteUrl]): Future[PackageInfo] = {
    metadata match {
      case metadataNormal: MetadataNormal =>
        val gitHubUrl = GitHub.gitHubUrl(s"https://github.com/${metadataNormal.repo}")
        gitHubUrl.flatMap(GitHub.gitHubGitUrl).fold(Future.failed[PackageInfo], { gitHubGitUri =>
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
        })
      case metadataNpm: MetadataNpm =>
        for {
          packageInfo <- npm.info(metadataNpm.packageName, version, maybeSourceUri)
          licenseMetadata <- license(metadataNpm)
        } yield packageInfo.copy(metadataLicenses = Seq(licenseMetadata))
    }
  }

  def archiveFromMetadata(metadata: Metadata, version: Version): Future[InputStream] = {
    metadata match {
      case metadataNormal: MetadataNormal =>
        metadataNormal.download.fold {
          downloadExists(("https://github.com" / metadataNormal.repo / "archive" / s"$version.zip").toString())
        } { download =>
          validDownload(download, version)
        } map { absoluteUrl =>
          val is = absoluteUrl.toJavaURI.toURL.openStream()
          if (absoluteUrl.toString().endsWith("tgz")) {
            new GZIPInputStream(is)
          }
          else {
            is
          }
        }
      case metadataNpm: MetadataNpm =>
        npm.archive(metadataNpm.packageName, version)
    }
  }

  def maybeBaseDirGlobFromMetadata(metadata: Metadata): Future[Option[String]] = {
    metadata match {
      case metadataNormal: MetadataNormal => Future.successful(metadataNormal.baseDir)
      case metadataNpm: MetadataNpm => npm.maybeBaseDirGlob(metadataNpm.packageName)
    }
  }

  def licensesFromMetadata(metadata: Metadata, version: Version, packageInfo: PackageInfo)(using ec: ExecutionContext): Future[Set[License]] = {
    licenses(metadata.id.toString, version, packageInfo)
  }
}

object Classic {
  sealed trait Metadata {
    val id: MavenCentral.ArtifactId
  }

  case class MetadataNormal(id: MavenCentral.ArtifactId, name: String, repo: String, download: Option[String], requireJsMain: Option[String], baseDir: Option[String]) extends Metadata

  case class MetadataNpm(id: MavenCentral.ArtifactId, packageName: String, licenseName: Option[String], licenseUrl: Option[String]) extends Metadata

  def parseMetadata(nameOrUrlish: String, propertiesString: String): Try[Metadata] = {
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
        for {
          name <- maybeName
          repo <- maybeRepo
        } yield MetadataNormal(MavenCentral.ArtifactId(nameOrUrlish), name, repo, maybeDownload, maybeRequireJsMain, maybeBaseDir)
      } { npmName =>
        Some(MetadataNpm(MavenCentral.ArtifactId(nameOrUrlish), npmName, maybeLicenseName, maybeLicenseUrl))
      }
    }.flatMap {
      case Some(metadata) => scala.util.Success(metadata)
      case None => scala.util.Failure(new Exception("Invalid properties file: missing required fields (name and repo for normal, or npm for npm-based)"))
    }
  }
}
