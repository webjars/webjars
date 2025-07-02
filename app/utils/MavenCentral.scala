package utils

import actors.{FetchWebJars, WebJarFetcher}
import com.google.inject.ImplementedBy
import models.{WebJar, WebJarVersion}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.spy.memcached.transcoders.{SerializingTranscoder, Transcoder}
import org.apache.pekko.actor._
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import play.api.http.Status
import play.api.libs.ws.WSClient
import play.api.{Configuration, Environment, Logging, Mode}

import java.io.FileNotFoundException
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try
import scala.xml.Elem

@ImplementedBy(classOf[MavenCentralLive])
trait MavenCentral {
  import MavenCentral._

  def fetchWebJars(groupId: GroupId): Future[Set[WebJar]]
  def fetchPom(gav: GAV, maybeUrlPrefix: Option[String] = None): Future[Elem]
  def webJars(groupId: GroupId): Future[List[WebJar]]
}

@Singleton
class MavenCentralLive @Inject() (memcache: Memcache, wsClient: WSClient, configuration: Configuration, webJarsFileService: WebJarsFileService, environment: Environment)
                                 (implicit ec: ExecutionContext, actorSystem: ActorSystem) extends MavenCentral with Logging {
  import MavenCentral._

  private lazy val maybeLimit = configuration.getOptional[Int]("mavencentral.limit").orElse(Option.when(environment.mode == Mode.Dev)(5))

  private implicit val transcoderOptionalInt: Transcoder[Option[Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Option[Int]]]
  private implicit val transcoderNameUrl: Transcoder[(String, String)] = new SerializingTranscoder().asInstanceOf[Transcoder[(String, String)]]

  private val browser: Browser = JsoupBrowser()

  def fetchWebJarNameAndUrl(gav: GAV): Future[(String, String)] = {
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
          fetchPom(gav.copy(artifactId = parentArtifactId)).map { parentXml =>
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
        (gav.artifactId, s"https://github.com/webjars/${gav.artifactId}")
    }
  }

  private def getWebJarNameAndUrl(gav: GAV): Future[(String, String)] = {
    memcache.getWithMiss[(String, String)](s"name-url-${gav.cacheKey}") {
      fetchWebJarNameAndUrl(gav)
    }
  }

  // cache filenotfounds so we don't keep looking them up
  def getNumFiles(gav: GAV): Future[Option[Int]] = {
    val cacheKey = s"numfiles-${gav.groupId}-${gav.artifactId}-${gav.version}"
    memcache.getWithMiss[Option[Int]](cacheKey) {
      webJarsFileService.getNumFiles(gav.groupId, gav.artifactId, gav.version).map { numFiles =>
        Some(numFiles)
      } recover {
        case _: FileNotFoundException => None
      }
    }
  }

  // this is trash but it is more reliable than search.maven.org
  //  because that site does severe rate limiting and has a low page size which results in lots of reqs needed to fetch the list of webjars
  //  and you'll get denied. and it won't feel good after you've spent hours writing this awfulness only to realize it won't work in production

  private def fetchDirs(url: String): Future[Set[String]] = {
    import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
    import net.ruippeixotog.scalascraper.dsl.DSL._

    Future.fromTry {
      Try {
        val as = browser.get(url) >> elementList("a")
        val hrefs = as.map(_.attr("href"))
        hrefs.filter(_.endsWith("/")).map(_.stripSuffix("/")).toSet
      }
    }
  }

  def versions(groupId: GroupId, artifactId: ArtifactId): Future[Set[String]] = {
    val groupPath = groupId.split('.').mkString("/")
    val url = s"https://repo1.maven.org/maven2/$groupPath/$artifactId/maven-metadata.xml"

    wsClient.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.fromTry {
            Try {
              (response.xml \\ "version").map(_.text).toSet
            }
          }
        case Status.NOT_FOUND =>
          Future.failed(new FileNotFoundException(url))
        case _ =>
          Future.failed(ServerError(response.body, response.status))
      }
    }
  }

  def artifactIds(groupId: GroupId): Future[Set[String]] = {
    val groupPath = groupId.split('.').mkString("", "/", "/")
    val groupUrl = s"https://repo1.maven.org/maven2/$groupPath"

    fetchDirs(groupUrl).flatMap { dirs =>
      val artifactIds = dirs.filterNot(_.startsWith("webjars-")).filterNot(_ == "2.11.2").filterNot(_ == "..") // workarounds

      val parentParts = groupId.split('.')
      versions(parentParts.init.mkString("."), parentParts.last).map { maybeNotArtifacts =>
        val good = artifactIds -- maybeNotArtifacts

        // with the limit, sort before taking so the output is more stable
        maybeLimit.fold(good)(good.toList.sortBy(_.toLowerCase).take(_).toSet)
      }
    }
  }

  private def gavsToWebJarVersion(gavs: Set[GAV]): Future[Set[WebJarVersion]] = {
    Future.sequence {
      gavs.map { gav =>
        getNumFiles(gav).map { maybeNumFiles =>
          maybeNumFiles.map { numFiles =>
            WebJarVersion(gav.version, Some(numFiles))
          }
        } recover {
          // include errors since we don't know yet if it is a valid webjar
          case _: Exception =>
            Some(WebJarVersion(gav.version, None))
        }
      }
    }.map(_.flatten)
  }

  private def convertToFutureOption[A](opt: Option[Future[A]])(implicit ec: ExecutionContext): Future[Option[A]] = {
    opt match {
      case Some(future) => future.map(Some(_))
      case None => Future.successful(None)
    }
  }

  private def fetchDetails(groupId: GroupId, artifactId: ArtifactId): Future[Option[WebJar]] = {
    versions(groupId, artifactId).flatMap { versions =>
      val gavs = versions.map(GAV(groupId, artifactId, _))

      gavsToWebJarVersion(gavs).flatMap { versions =>
        val sorted = versions.toSeq.sortBy(_.number)(VersionStringOrdering.compare).reverse
        convertToFutureOption {
          sorted.headOption.map { latest =>
            getWebJarNameAndUrl(GAV(groupId, artifactId, latest.number)).map { case (name, url) =>
              WebJar(groupId, artifactId, name, url, sorted)
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

  def fetchWebJars(groupId: GroupId): Future[Set[WebJar]] = {
    logger.info(s"Getting $groupId WebJars")

    artifactIds(groupId).flatMap { artifactIds =>
      Future.sequence {
        artifactIds.map { artifactId =>
          fetchDetails(groupId, artifactId)
        }
      }.map(_.flatten)
    }.map { webJars =>
      logger.info(s"Retrieved ${webJars.size} $groupId WebJars")
      webJars
    }
  }

  def webJars(groupId: GroupId): Future[List[WebJar]] = {
    actorSystem.actorSelection("user/" + groupId).resolveOne(1.second).flatMap { _ =>
      // in-flight request exists
      Future.failed(new ExistingWebJarRequestException(groupId))
    } recoverWith {
      // no request so make one
      case _: ActorNotFound =>
        implicit val timeout: Timeout = Timeout(10.minutes)

        val webJarFetcherTry = Future.fromTry(Try(actorSystem.actorOf(Props(classOf[WebJarFetcher], this, ec), groupId))).recoverWith {
          case _: InvalidActorNameException => Future.failed(new ExistingWebJarRequestException(groupId))
        }

        webJarFetcherTry.flatMap { webJarFetcher =>
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars(groupId)).mapTo[Set[WebJar]]

          fetchWebJarsFuture.onComplete(_ => actorSystem.stop(webJarFetcher))

          fetchWebJarsFuture.map(_.toList.sortBy(_.name.toLowerCase))
        }
    }
  }

  def fetchPom(gav: GAV, maybeUrlPrefix: Option[String] = None): Future[Elem] = {
    val urlPrefix = maybeUrlPrefix.getOrElse("https://repo1.maven.org/maven2")
    val url = s"$urlPrefix/${gav.path}.pom"
    wsClient.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.fromTry(Try(response.xml))
        case Status.NOT_FOUND =>
          Future.failed(new FileNotFoundException(url))
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }
}


object MavenCentral {
  class ExistingWebJarRequestException(groupId: GroupId) extends RuntimeException(s"There is an existing WebJar request for $groupId")

  type GroupId = String
  type ArtifactId = String

  case class GAV(groupId: GroupId, artifactId: ArtifactId, version: String) {
    private lazy val groupIdPath = groupId.replace(".", "/")
    lazy val path = s"$groupIdPath/$artifactId/$version/$artifactId-$version"
    lazy val cacheKey = s"$groupId-$artifactId-$version"

    override def toString: String = s"$groupId:$artifactId:$version"
  }
}
