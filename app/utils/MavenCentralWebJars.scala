package utils

import actors.{FetchWebJars, WebJarFetcher}
import com.google.inject.ImplementedBy
import com.jamesward.zio_mavencentral.MavenCentral
import models.{WebJar, WebJarVersion}
import net.spy.memcached.transcoders.{SerializingTranscoder, Transcoder}
import org.apache.pekko.actor.*
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import play.api.{Configuration, Environment, Logging, Mode}
import utils.Adapter.*
import zio.http.Client
import zio.{Scope, ZLayer}

import java.io.FileNotFoundException
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try
import scala.xml.Elem

@ImplementedBy(classOf[MavenCentralWebJarsLive])
trait MavenCentralWebJars {
  def fetchPom(gav: MavenCentral.GroupArtifactVersion): Future[Elem]
  def fetchWebJars(groupId: MavenCentral.GroupId): Future[Set[WebJar]]
  def webJars(groupId: MavenCentral.GroupId): Future[List[WebJar]]
}

@Singleton
class MavenCentralWebJarsLive @Inject() (memcache: Memcache, configuration: Configuration, webJarsFileService: WebJarsFileService, environment: Environment)
                                 (using ec: ExecutionContext, actorSystem: ActorSystem) extends MavenCentralWebJars with Logging {

  private lazy val maybeLimit = configuration.getOptional[Int]("mavencentral.limit").orElse(Option.when(environment.mode == Mode.Dev)(5))

  private given Transcoder[Option[Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Option[Int]]]
  private given Transcoder[(String, String)] = new SerializingTranscoder().asInstanceOf[Transcoder[(String, String)]]

  extension (gav: MavenCentral.GroupArtifactVersion)
    def cacheKey = s"${gav.groupId}-${gav.artifactId}-${gav.version}"

  private val mavenCentralLayer: ZLayer[Any, Nothing, Scope & Client] = Client.default.orDie ++ Scope.default

  def fetchPom(gav: MavenCentral.GroupArtifactVersion): Future[Elem] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version).mapError {
      case e: MavenCentral.NotFoundError => FileNotFoundException("pom not found")
      case t: Throwable => t
    }.runToFuture(mavenCentralLayer)

  def fetchWebJarNameAndUrl(gav: MavenCentral.GroupArtifactVersion): Future[(String, String)] = {
    fetchPom(gav).flatMap { xml =>
      val artifactId = (xml \ "artifactId").text
      val rawName = (xml \ "name").text
      val name = if (rawName.contains("${") || rawName.isEmpty) {
        // can't handle pom properties so fallback to id
        artifactId
      } else {
        rawName
      }
      val rawUrl = (xml \ "scm" \ "url").text
      val urlFuture = if (rawUrl.contains("${")) {
        // can't handle pom properties so fallback to a guess
        Future.successful(s"https://github.com/webjars/$artifactId")
      } else {
        if (rawUrl != "") {
          Future.successful(rawUrl)
        }
        else {
          // try the parent pom
          val parentArtifactId = (xml \ "parent" \ "artifactId").text
          fetchPom(gav.copy(artifactId = MavenCentral.ArtifactId(parentArtifactId))).map { parentXml =>
            (parentXml \ "scm" \ "url").text
          }
        }
      }

      urlFuture.map { url =>
        (name, url)
      }

    } recover {
      case _ =>
        // fall back to the usual
        (gav.artifactId.toString, s"https://github.com/webjars/${gav.artifactId}")
    }
  }

  private def getWebJarNameAndUrl(gav: MavenCentral.GroupArtifactVersion): Future[(String, String)] = {
    memcache.getWithMiss[(String, String)](s"name-url-${gav.cacheKey}") {
      fetchWebJarNameAndUrl(gav)
    }
  }

  // cache filenotfounds so we don't keep looking them up
  def getNumFiles(gav: MavenCentral.GroupArtifactVersion): Future[Option[Int]] = {
    val cacheKey = s"numfiles-${gav.groupId}-${gav.artifactId}-${gav.version}"
    memcache.getWithMiss[Option[Int]](cacheKey) {
      webJarsFileService.getNumFiles(gav).map { numFiles =>
        Some(numFiles)
      } recover {
        case _: FileNotFoundException => None
      }
    }
  }

  def fetchVersions(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): Future[MavenCentral.SeqWithLastModified[MavenCentral.Version]] =
    MavenCentral.searchVersions(groupId, artifactId).mapError {
      case e: MavenCentral.GroupIdOrArtifactIdNotFoundError => Throwable("groupId or artifactId not found")
      case t: Throwable => t
    }.runToFuture(mavenCentralLayer)

  private def gavsToWebJarVersion(gavs: Set[MavenCentral.GroupArtifactVersion]): Future[Set[WebJarVersion]] = {
    Future.sequence {
      gavs.map { gav =>
        getNumFiles(gav).map { maybeNumFiles =>
          maybeNumFiles.map { numFiles =>
            WebJarVersion(gav.version.toString, Some(numFiles))
          }
        } recover {
          // include errors since we don't know yet if it is a valid webjar -
          case _: Exception =>
            Some(WebJarVersion(gav.version.toString, None))
        }
      }
    }.map(_.flatten)
  }


  private def convertToFutureOption[A](opt: Option[Future[A]])(using ec: ExecutionContext): Future[Option[A]] = {
    opt match {
     case Some(future) => future.map(Some(_))
     case None => Future.successful(None)
    }
  }


  private def fetchDetails(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): Future[Option[WebJar]] = {
    fetchVersions(groupId, artifactId).flatMap { versions =>
      val gavs = versions.items.map(MavenCentral.GroupArtifactVersion(groupId, artifactId, _)).toSet

      gavsToWebJarVersion(gavs).flatMap { versions =>
        val sorted = versions.toSeq.sortBy(_.number)(using VersionStringOrdering).reverse
        convertToFutureOption {
          sorted.headOption.map { latest =>
            getWebJarNameAndUrl(MavenCentral.GroupArtifactVersion(groupId, artifactId, MavenCentral.Version(latest.number))).map { case (name, url) =>
              WebJar(groupId.toString, artifactId.toString, name, url, sorted)
            }
          }
        }
      }
    } recover {
      // if we couldn't find versions, this isn't a valid WebJar
      case _: FileNotFoundException =>
        None
    }
  }

  def fetchArtifactIds(groupId: MavenCentral.GroupId): Future[MavenCentral.SeqWithLastModified[MavenCentral.ArtifactId]] = {
    val artifactIds = if groupId.toString == "org.webjars.npm" then
      // there is a org.webjars:npm artifact that makes it so we need to exlude versions from the list of artifacts
      MavenCentral.searchArtifacts(groupId).zipPar(MavenCentral.mavenMetadata(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("npm"))).map { case (npmArtifacts, npmMetadata) =>
        val versions = npmMetadata \ "versioning" \ "versions" \ "version"
        val workaroundVersions = versions.map(_.text) :+ "2.11.2" // for some reason the org.webjars:npm:2.11.2 artifact exists in Maven Central but not in the metadata
        npmArtifacts.copy(items = npmArtifacts.items.filterNot(workaroundVersions.contains(_)))
      }
    else {
      // non-webjars exist in the org.webjars groupId
      MavenCentral.searchArtifacts(groupId).map { artifacts =>
        artifacts.copy(items = artifacts.items.filterNot(_.toString.startsWith("webjars-")))
      }
    }

    artifactIds.mapError {
      case _: MavenCentral.GroupIdNotFoundError | _: MavenCentral.GroupIdOrArtifactIdNotFoundError => Throwable("groupId not found")
      case t: Throwable => t
    }.runToFuture(mavenCentralLayer)
  }

  def fetchWebJars(groupId: MavenCentral.GroupId): Future[Set[WebJar]] = {
    logger.info(s"Getting $groupId WebJars")

    fetchArtifactIds(groupId).flatMap { artifactIds =>
      val artifactIdsToFetch = maybeLimit.fold(artifactIds.items)(artifactIds.items.toList.sortBy(_.toString.toLowerCase).take(_).toSet)

      Future.sequence {
        artifactIdsToFetch.map { artifactId =>
          fetchDetails(groupId, artifactId)
        }
      }.map(_.flatten)
    }.map { webJars =>
      logger.info(s"Retrieved ${webJars.size} $groupId WebJars")
      webJars.toSet
    }
  }

  def webJars(groupId: MavenCentral.GroupId): Future[List[WebJar]] = {
    actorSystem.actorSelection("user/" + groupId).resolveOne(1.second).flatMap { _ =>
      // in-flight request exists
      Future.failed(MavenCentralWebJars.ExistingWebJarRequestException(groupId))
    } recoverWith {
      // no request so make one
      case _: ActorNotFound =>
        implicit val timeout: Timeout = Timeout(10.minutes)

        val webJarFetcherTry = Future.fromTry(Try(actorSystem.actorOf(Props(classOf[WebJarFetcher], this, ec), groupId.toString))).recoverWith {
          case _: InvalidActorNameException => Future.failed(MavenCentralWebJars.ExistingWebJarRequestException(groupId))
        }

        webJarFetcherTry.flatMap { webJarFetcher =>
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars(groupId)).mapTo[Set[WebJar]]

          fetchWebJarsFuture.onComplete(_ => actorSystem.stop(webJarFetcher))

          fetchWebJarsFuture.map(_.toList.sortBy(_.name.toLowerCase))
        }
    }
  }
}


object MavenCentralWebJars {
  class ExistingWebJarRequestException(groupId: MavenCentral.GroupId) extends RuntimeException(s"There is an existing WebJar request for $groupId")
}
