package utils

import javax.inject.Inject

import actors.{FetchWebJars, WebJarFetcher}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models.{WebJar, WebJarVersion}
import org.joda.time.DateTime
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.json._
import play.api.libs.ws.{WSAuthScheme, WSClient}
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

  lazy val ossUsername = configuration.getString("oss.username").get
  lazy val ossPassword = configuration.getString("oss.password").get
  lazy val ossProject = configuration.getString("oss.project").get

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

  def webJarsFromJson(groupId: String)(json: JsValue): Future[List[WebJar]] = {

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
          val cacheKey = s"numfiles-$groupId-$artifactId-$version"
          memcache.instance.get[Int](cacheKey).flatMap { maybeNumFiles =>
            maybeNumFiles.fold {
              val numFilesFuture = webJarsFileService.getNumFiles(groupId, artifactId, version)
              numFilesFuture.foreach(numFiles => memcache.instance.set(cacheKey, numFiles, Duration.Inf))
              numFilesFuture
            } (Future.successful) map { numFiles =>
              WebJarVersion(version, numFiles)
            }
          } recover {
            case e: Exception =>
              Logger.error(s"Error fetching file list for $groupId $artifactId $version", e)
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

            fetchWebJarNameAndUrl(groupId, artifactId, latestVersion).map {
              case (name, url) =>
                WebJar(groupId, artifactId, name, url, webJarVersions)
            }
        }
      }
    } map { webJars =>
      webJars.toList.sortWith(_.name.toLowerCase < _.name.toLowerCase)
    }

    webJarsFuture
  }

  def fetchWebJars(groupId: String): Future[List[WebJar]] = {

    Logger.info("Getting the WebJars for " + groupId)

    val searchUrl = configuration.getString("webjars.searchGroupUrl").get.format(groupId)

    wsClient.url(searchUrl).get().flatMap { response =>
      Try(response.json).map(webJarsFromJson(groupId)).getOrElse(Future.failed(new MavenCentral.UnavailableException(response.body)))
    }

  }

  def webJars(groupId: String): Future[List[WebJar]] = {
    cache.get[List[WebJar]](groupId, 1.hour) {
      // todo: for some reason this blocks longer than 1 second if things are busy
      actorSystem.actorSelection("user/" + groupId).resolveOne(1.second).flatMap { actorRef =>
        // in-flight request exists
        Future.failed(new Exception("Existing request for WebJars"))
      } recoverWith {
        // no request so make one
        case e: ActorNotFound =>
          implicit val timeout = Timeout(10.minutes)
          val webJarFetcher = actorSystem.actorOf(Props(classOf[WebJarFetcher], this, ec), groupId)
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars(groupId)).mapTo[List[WebJar]]
          fetchWebJarsFuture.onFailure {
            case e: Exception =>
              actorSystem.stop(webJarFetcher)
              Logger.error(s"WebJar fetch failed for $groupId: ${e.getMessage}", e)
          }
          fetchWebJarsFuture.foreach { fetchedWebJars =>
            Logger.info(s"WebJar fetch complete for $groupId")
            actorSystem.stop(webJarFetcher)
          }
          // fail cause this is will likely take a long time
          //Future.failed(new Exception("Making new request for WebJars"))
          fetchWebJarsFuture
      }
    }
  }

  def webJars: Future[List[WebJar]] = {
    val npmFuture = webJars(NPM.groupId)
    val bowerFuture = webJars(Bower.groupId)
    val classicFuture = webJars(Classic.groupId)

    for {
      npmWebJars <- npmFuture
      bowerWebJars <- bowerFuture
      classicWebJars <- classicFuture
    } yield npmWebJars ++ bowerWebJars ++ classicWebJars
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

  def getStats(groupId: String, dateTime: DateTime): Future[Seq[(String, String, Int)]] = {
    val queryString = Seq(
      "p" -> ossProject,
      "g" -> groupId,
      "t" -> "raw",
      "from" -> dateTime.toString("yyyyMM"),
      "nom" -> "1"
    )

    val statsFuture = wsClient.url("https://oss.sonatype.org/service/local/stats/slices")
      .withAuth(ossUsername, ossPassword, WSAuthScheme.BASIC)
      .withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON)
      .withQueryString(queryString:_*)
      .get()

    statsFuture.flatMap { response =>
      response.status match {
        case Status.OK =>
          val total = (response.json \ "data" \ "total").as[Int]

          if (total > 0) {

            val slices = (response.json \ "data" \ "slices").as[Seq[JsObject]]
            val webJarCounts = slices.map { jsObject =>
              val name = (jsObject \ "name").as[String]
              val count = (jsObject \ "count").as[Int]
              (groupId, name, count)
            }

            val sorted = webJarCounts.sortBy(_._3)(Ordering[Int].reverse)

            Future.successful(sorted)
          }
          else {
            Future.failed(new Exception("Stats were empty"))
          }
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

  def getStats(dateTime: DateTime): Future[Seq[(String, String, Int)]] = {
    val classicStatsFuture = getStats(Classic.groupId, dateTime)
    val bowerStatsFuture = getStats(Bower.groupId, dateTime)
    val npmStatsFuture = getStats(NPM.groupId, dateTime)

    for {
      classicStats <- classicStatsFuture
      bowerStats <- bowerStatsFuture
      npmStats <- npmStatsFuture
    } yield npmStats ++ bowerStats ++ classicStats
  }

  def mostDownloaded(groupId: String, dateTime: DateTime, num: Int): Future[Seq[(String, String, Int)]] = {
    getStats(groupId, dateTime).map(_.take(num))
  }

  def mostDownloaded(dateTime: DateTime, num: Int): Future[Seq[(String, String, Int)]] = {
    val mostDownloadedNpmFuture = mostDownloaded(NPM.groupId, dateTime, num)
    val mostDownloadedBowerFuture = mostDownloaded(Bower.groupId, dateTime, num)
    val mostDownloadedClassicFuture = mostDownloaded(Classic.groupId, dateTime, num)

    for {
      mostDownloadedClassic <- mostDownloadedClassicFuture
      mostDownloadedBower <- mostDownloadedBowerFuture
      mostDownloadedNpm <- mostDownloadedNpmFuture
    } yield mostDownloadedNpm ++ mostDownloadedBower ++ mostDownloadedClassic
  }

}


object MavenCentral {
  class UnavailableException(msg: String) extends RuntimeException(msg)
}
