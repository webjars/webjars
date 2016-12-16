package utils

import javax.inject.Inject

import actors.{FetchWebJars, WebJarFetcher}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models.WebJarCatalog.WebJarCatalog
import models.{WebJar, WebJarCatalog, WebJarVersion}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}
import shade.memcached.MemcachedCodecs._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.xml.Elem

class MavenCentral @Inject() (cache: Cache, memcache: Memcache, wsClient: WSClient, actorSystem: ActorSystem, configuration: Configuration, webJarsFileService: WebJarsFileService) (implicit ec: ExecutionContext) {

  lazy val webJarFetcher: ActorRef = actorSystem.actorOf(Props[WebJarFetcher])

  implicit val webJarVersionReads = Json.reads[WebJarVersion]
  implicit val webJarVersionWrites = Json.writes[WebJarVersion]

  implicit val webJarReads = Json.reads[WebJar]
  implicit val webJarWrites = Json.writes[WebJar]

  def fetchWebJarNameAndUrl(groupId: String, artifactId: String, version: String): Future[(String, String)] = {
    getPom(groupId, artifactId, version).flatMap { xml =>
      val artifactId = (xml \ "artifactId").text
      val rawName = (xml \ "name").text
      val name = if (rawName.contains("${") || (rawName.length == 0)) {
        // can't handle pom properties so fallback to id
        artifactId
      } else {
        rawName
      }
      val rawUrl = (xml \ "scm" \ "url").text
      val urlFuture = if (rawUrl.contains("${")) {
        // can't handle pom properties so fallback to a guess
        Future.successful(s"http://github.com/webjars/$artifactId")
      } else {
        if (rawUrl != "") {
          Future.successful(rawUrl)
        }
        else {
          // try the parent pom
          val parentArtifactId = (xml \ "parent" \ "artifactId").text
          getPom(groupId, parentArtifactId, version).map { parentXml =>
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
        (artifactId, s"http://github.com/webjars/$artifactId")
    }
  }

  def webJarsFromJson(catalog: WebJarCatalog)(json: JsValue): Future[List[WebJar]] = {

    val allVersions = (json \ "response" \ "docs").as[List[JsObject]].map { jsObject =>
      ((jsObject \ "a").as[String], (jsObject \ "v").as[String])
    }

    // group by the artifactId
    val artifactsAndVersions: Map[String, List[String]] = allVersions.groupBy(_._1).filterKeys(!_.startsWith("webjars-")).mapValues(_.map(_._2))

    // partition and batch

    def fetchWebJarVersions(artifactAndVersions: (String, List[String])): (String, Future[List[WebJarVersion]]) = {
      val (artifactId, versions) = artifactAndVersions
      val versionsFuture = Future.sequence {
        versions.map { version =>
          val cacheKey = s"numfiles-${catalog.toString}-$artifactId-$version"
          memcache.instance.get[Int](cacheKey).flatMap { maybeNumFiles =>
            maybeNumFiles.fold {
              val numFilesFuture = webJarsFileService.getNumFiles(catalog.toString, artifactId, version)
              numFilesFuture.foreach(numFiles => memcache.instance.set(cacheKey, numFiles, Duration.Inf))
              numFilesFuture
            } (Future.successful) map { numFiles =>
              WebJarVersion(version, numFiles)
            }
          } recover {
            case e: Exception =>
              Logger.error(s"Error fetching file list for ${catalog.toString} $artifactId $version", e)
              WebJarVersion(version, 0)
          }
        }
      } map { webJarVersions =>
        webJarVersions.sorted.reverse
      }

      artifactId -> versionsFuture
    }

    def processBatch(resultsFuture: Future[Map[String, List[WebJarVersion]]], batch: Map[String, List[String]]): Future[Map[String, List[WebJarVersion]]] = {
      resultsFuture.flatMap { results =>
        val batchFutures: Map[String, Future[List[WebJarVersion]]] = batch.map(fetchWebJarVersions)
        val batchFuture: Future[Map[String, List[WebJarVersion]]] = Future.traverse(batchFutures) {
          case (artifactId, futureVersions) =>
            futureVersions.map(artifactId -> _)
        }.map(_.toMap)

        batchFuture.map { batchResult =>
          results ++ batchResult
        }
      }
    }

    // batch size = 100
    val artifactsWithWebJarVersionsFuture: Future[Map[String, List[WebJarVersion]]] = artifactsAndVersions.grouped(100).foldLeft(Future.successful(Map.empty[String, List[WebJarVersion]]))(processBatch)

    val webJarsFuture: Future[List[WebJar]] = artifactsWithWebJarVersionsFuture.flatMap { artifactsWithWebJarVersions =>
      Future.sequence {
        artifactsWithWebJarVersions.map {
          case (artifactId, webJarVersions) =>
            val latestVersion = webJarVersions.map(_.number).head

            fetchWebJarNameAndUrl(catalog.toString, artifactId, latestVersion).map {
              case (name, url) =>
                WebJar(catalog.toString, artifactId, name, url, webJarVersions)
            }
        }
      }
    } map { webJars =>
      webJars.toList.sortWith(_.name.toLowerCase < _.name.toLowerCase)
    }

    webJarsFuture
  }

  def fetchWebJars(catalog: WebJarCatalog): Future[List[WebJar]] = {

    Logger.info("Getting the WebJars for " + catalog.toString)

    val searchUrl = configuration.getString("webjars.searchGroupUrl").get.format(catalog.toString)

    wsClient.url(searchUrl).get().flatMap { response =>
      Try(response.json).map(webJarsFromJson(catalog)).getOrElse(Future.failed(new MavenCentral.UnavailableException(response.body)))
    }

  }

  def webJars(catalog: WebJarCatalog): Future[List[WebJar]] = {
    cache.get[List[WebJar]](catalog.toString, 1.hour) {
      // todo: for some reason this blocks longer than 1 second if things are busy
      actorSystem.actorSelection("user/" + catalog.toString).resolveOne(1.second).flatMap { actorRef =>
        // in-flight request exists
        Future.failed(new Exception("Existing request for WebJars"))
      } recoverWith {
        // no request so make one
        case e: ActorNotFound =>
          implicit val timeout = Timeout(10.minutes)
          val webJarFetcher = actorSystem.actorOf(Props(classOf[WebJarFetcher], this, ec), catalog.toString)
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars(catalog)).mapTo[List[WebJar]]
          fetchWebJarsFuture.onFailure {
            case e: Exception =>
              actorSystem.stop(webJarFetcher)
              Logger.error(s"WebJar fetch failed for ${catalog.toString}: ${e.getMessage}", e)
          }
          fetchWebJarsFuture.foreach { fetchedWebJars =>
            Logger.info(s"WebJar fetch complete for ${catalog.toString}")
            actorSystem.stop(webJarFetcher)
          }
          // fail cause this is will likely take a long time
          //Future.failed(new Exception("Making new request for WebJars"))
          fetchWebJarsFuture
      }
    }
  }

  def webJars: Future[List[WebJar]] = {
    val classicFuture = webJars(WebJarCatalog.CLASSIC)
    val bowerFuture = webJars(WebJarCatalog.BOWER)
    val npmFuture = webJars(WebJarCatalog.NPM)

    for {
      classicWebJars <- classicFuture
      bowerWebJars <- bowerFuture
      npmWebJars <- npmFuture
    } yield classicWebJars ++ bowerWebJars ++ npmWebJars
  }

  private def fetchPom(groupId: String, artifactId: String, version: String): Future[Elem] = {
    val groupIdPath = groupId.replace(".", "/")
    val url = s"http://repo1.maven.org/maven2/$groupIdPath/$artifactId/$version/$artifactId-$version.pom"
    wsClient.url(url).get().map(_.xml)
  }

  def getPom(groupId: String, artifactId: String, version: String): Future[Elem] = {
    val cacheKey = s"pom-$groupId-$artifactId"
    memcache.instance.get[Elem](cacheKey).flatMap { maybeElem =>
      maybeElem.map(Future.successful).getOrElse {
        val pomFuture = fetchPom(groupId, artifactId, version)
        pomFuture.flatMap { pom =>
          memcache.instance.set(cacheKey, pom, Duration.Inf).map(_ => pom)
        }
      }
    }
  }

}


object MavenCentral {
  class UnavailableException(msg: String) extends RuntimeException(msg)
}
