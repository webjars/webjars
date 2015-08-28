package utils

import actors.{FetchWebJars, WebJarFetcher}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models.WebJarCatalog.WebJarCatalog
import models.{WebJarCatalog, WebJar, WebJarVersion}
import play.api.Play.current
import play.api.cache.Cache
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.{Logger, Play}
import Memcache._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.xml.Elem

object MavenCentral {

  implicit val ec: ExecutionContext = Akka.system(Play.current).dispatchers.lookup("mavencentral.dispatcher")

  lazy val webJarFetcher: ActorRef = Akka.system.actorOf(Props[WebJarFetcher])

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

  def fetchWebJars(catalog: WebJarCatalog): Future[List[WebJar]] = {

    Logger.info("Getting the WebJars for " + catalog.toString)

    val searchUrl = Play.configuration.getString("webjars.searchGroupUrl").get.format(catalog.toString)

    WS.url(searchUrl).get().flatMap { response =>

      val allVersions = (response.json \ "response" \ "docs").as[List[JsObject]].map { jsObject =>
        ((jsObject \ "a").as[String], (jsObject \ "v").as[String])
      }

      // group by the artifactId
      val grouped: Map[String, List[String]] = allVersions.groupBy(_._1).filterKeys(!_.startsWith("webjars-")).mapValues(_.map(_._2))

      val webJarsWithFutureVersions: Map[String, Future[List[WebJarVersion]]] = grouped.map {
        case (artifactId, versions) =>
          val webJarVersionsFuture = Future.sequence {
            versions.map { version =>
              WebJarsFileService.getFileList(catalog.toString, artifactId, version).map { fileList =>
                WebJarVersion(version, fileList.length)
              }.recover {
                case e: Exception =>
                  Logger.error(e.getMessage)
                  WebJarVersion(version, 0)
              }
            }
          }.map { webJarVersions =>
            webJarVersions.sorted.reverse
          }
          artifactId -> webJarVersionsFuture
      }

      val webJarsFuture: Future[List[WebJar]] = Future.traverse(webJarsWithFutureVersions) {
        case (artifactId, webJarVersionsFuture) =>
          webJarVersionsFuture.flatMap { webJarVersions =>
            val latestVersion = webJarVersions.map(_.number).head

            MavenCentral.fetchWebJarNameAndUrl(catalog.toString, artifactId, latestVersion).map {
              case (name, url) =>
                WebJar(catalog.toString, artifactId, name, url, webJarVersions)
            }
          }
      } map { webJars =>
        webJars.toList.sortWith(_.name.toLowerCase < _.name.toLowerCase)
      }

      webJarsFuture
    }
  }

  def webJars(catalog: WebJarCatalog): Future[List[WebJar]] = {
    Cache.getAs[List[WebJar]](catalog.toString).map(Future.successful).getOrElse {
      // todo: for some reason this blocks longer than 1 second if things are busy
      Akka.system.actorSelection("user/" + catalog.toString).resolveOne(1.second).flatMap { actorRef =>
        // in-flight request exists
        Future.failed(new Exception("Existing request for WebJars"))
      } recoverWith {
        // no request so make one
        case e: ActorNotFound =>
          implicit val timeout = Timeout(10.minutes)
          val webJarFetcher = Akka.system.actorOf(Props(classOf[WebJarFetcher], catalog), catalog.toString)
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars).mapTo[List[WebJar]]
          fetchWebJarsFuture.onFailure {
            case e: Exception =>
              Logger.error(s"WebJar fetch failed for ${catalog.toString}: ${e.getMessage}")
              e.getStackTrace.foreach { t => Logger.error(t.toString) }
          }
          fetchWebJarsFuture.foreach { fetchedWebJars =>
            Logger.info(s"WebJar fetch complete for ${catalog.toString}")
            Akka.system.stop(webJarFetcher)
            Cache.set(catalog.toString, fetchedWebJars, 1.hour)
          }
          // fail cause this is will likely take a long time
          Future.failed(new Exception("Making new request for WebJars"))
      }
    }
  }

  def webJars: Future[List[WebJar]] = {
    val classicFuture = MavenCentral.webJars(WebJarCatalog.CLASSIC)
    val bowerFuture = MavenCentral.webJars(WebJarCatalog.BOWER)
    val npmFuture = MavenCentral.webJars(WebJarCatalog.NPM)

    for {
      classicWebJars <- classicFuture
      bowerWebJars <- bowerFuture
      npmWebJars <- npmFuture
    } yield classicWebJars ++ bowerWebJars ++ npmWebJars
  }

  private def fetchPom(groupId: String, artifactId: String, version: String): Future[Elem] = {
    val groupIdPath = groupId.replace(".", "/")
    val url = s"http://repo1.maven.org/maven2/$groupIdPath/$artifactId/$version/$artifactId-$version.pom"
    WS.url(url).get().map(_.xml)
  }

  def getPom(groupId: String, artifactId: String, version: String): Future[Elem] = {
    val cacheKey = s"pom-$groupId-$artifactId"
    Global.memcached.get[Elem](cacheKey).flatMap { maybeElem =>
      maybeElem.map(Future.successful).getOrElse {
        val pomFuture = fetchPom(groupId, artifactId, version)
        pomFuture.flatMap { pom =>
          Global.memcached.set(cacheKey, pom, Duration.Inf).map(_ => pom)
        }
      }
    }
  }

}
