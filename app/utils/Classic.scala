package utils

import play.api.http.Status
import play.api.i18n.{Langs, MessagesApi}
import play.api.libs.concurrent.Futures
import play.api.libs.json.JsObject
import play.api.libs.ws.WSClient
import utils.Deployable.{NameOrUrlish, Version}

import java.io.{InputStream, StringReader}
import java.net.{URI, URL}
import java.util.Properties
import javax.inject.Inject
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Classic @Inject() (ws: WSClient, val licenseDetector: LicenseDetector, val messages: MessagesApi, val langs: Langs, gitHub: GitHub, cache: Cache)(implicit ec: ExecutionContext) extends Deployable {
  override val name: String = "Classic"
  override val groupIdQuery: String = "org.webjars"
  override def includesGroupId(groupId: String): Boolean = groupId.equalsIgnoreCase("org.webjars")

  case class Metadata(id: String, name: String, repo: String, requireJsMain: Option[String], baseDir: Option[String])

  def metadata(nameOrUrlish: NameOrUrlish): Future[Metadata] = {
    gitHub.raw(new URL("https://github.com/webjars/webjars-classic"), "main", s"$nameOrUrlish.properties")
      .recoverWith {
        case _ =>
          Future.failed(
            new Exception(s"The Classic WebJar $nameOrUrlish does not support this")
          )
      }
      .flatMap { propertiesString =>
        Future.fromTry(
          Try {
            val properties = new Properties()
            properties.load(new StringReader(propertiesString))
            properties
          }
        ).flatMap { properties =>
          val maybeName = Option(properties.getProperty("name"))
          val maybeRepo = Option(properties.getProperty("repo"))
          val maybeRequireJsMain = Option(properties.getProperty("requirejs.main"))
          val maybeBaseDir = Option(properties.getProperty("base.dir"))

          val maybeMetadata = for {
            name <- maybeName
            repo <- maybeRepo
          } yield Metadata(nameOrUrlish, name, repo, maybeRequireJsMain, maybeBaseDir)

          maybeMetadata.fold(Future.failed[Metadata](new Exception("properties file was invalid")))(Future.successful(_))
        }
    }
  }

  override def groupId(nameOrUrlish: NameOrUrlish, version: Version): Future[String] = Future.successful("org.webjars")

  override def artifactId(nameOrUrlish: NameOrUrlish, version: Version): Future[String] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.map(_.id)
  }

  override def excludes(nameOrUrlish: NameOrUrlish, version: Version): Future[Set[String]] = Future.successful(Set.empty)

  override val metadataFile: Option[String] = None
  override def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): Future[Option[String]] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.map(_.baseDir)
  }

  override def pathPrefix(nameOrUrlish: NameOrUrlish, releaseVersion: Version, packageInfo: PackageInfo): Future[String] =
    Future.successful(s"$nameOrUrlish/$releaseVersion")

  def license(metadata: Metadata): Future[String] = {
    val req = gitHub.maybeAuthToken.fold(
      ws.url(s"https://api.github.com/repos/${metadata.repo}/license")
    )( accessToken =>
      gitHub.ws(s"repos/${metadata.repo}/tags", accessToken)
    )
    req.get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful(
            (response.json \ "license" \ "spdx_id").as[String]
          )
        case _ =>
          Future.failed(ServerError(response.body, response.status))
      }
    }
  }

  override def info(nameOrUrlish: NameOrUrlish, version: Version, maybeSourceUri: Option[URI]): Future[PackageInfo] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap { metadata =>
      val gitHubUrl = GitHub.gitHubUrl(s"https://github.com/${metadata.repo}")
      gitHubUrl.flatMap(GitHub.gitHubGitUri).fold(Future.failed[PackageInfo], { gitHubGitUri =>
        license(metadata).map { license =>
          PackageInfo(
            metadata.name,
            version.vless,
            gitHubUrl.toOption,
            gitHubGitUri,
            gitHubUrl.flatMap(GitHub.gitHubIssuesUrl).toOption,
            Seq(license),
            Map.empty, // todo: support dependencies via properties
            Map.empty,
            Some(version),
          )
        }
      })
    }
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] =
    Future.successful(Set.empty)

  override def archive(nameOrUrlish: NameOrUrlish, version: Version): Future[InputStream] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.map { metadata =>
      new URL(s"https://github.com/${metadata.repo}/archive/refs/tags/$version.zip").openStream()
    }
  }

  override def file(nameOrUrlish: NameOrUrlish, version: Version, filename: String): Future[String] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap { metadata =>
      GitHub.gitHubUrl(s"https://github.com/${metadata.repo}").fold(Future.failed[String], { gitHubUrl =>
        gitHub.raw(gitHubUrl, version, filename)
      })
    }
  }

  override def versions(nameOrUrlish: NameOrUrlish): Future[Set[Version]] = {
    cache.get[Metadata](s"webjars-classic-$nameOrUrlish", 1.hour) {
      metadata(nameOrUrlish)
    }.flatMap { metadata =>
      val req = gitHub.maybeAuthToken.fold(
          ws.url(s"https://api.github.com/repos/${metadata.repo}/tags")
      )( accessToken =>
        gitHub.ws(s"repos/${metadata.repo}/tags", accessToken)
      )
      req.get().flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful(
              response.json.as[List[JsObject]].map { versionJson =>
                (versionJson \ "name").as[Version]
              }.toSet
            )
          case _ =>
            Future.failed(ServerError(response.body, response.status))
        }
      }
    }
  }

  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String])(implicit ec: ExecutionContext, futures: Futures): Future[Map[String, String]] = {
    Future.successful(Map.empty)
  }
}
