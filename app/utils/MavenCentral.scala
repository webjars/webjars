package utils

import java.io.FileNotFoundException

import actors.{FetchWebJars, WebJarFetcher}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import javax.inject.{Inject, Singleton}
import models.{WebJar, WebJarType, WebJarVersion}
import net.spy.memcached.transcoders.{IntegerTranscoder, SerializingTranscoder, Transcoder}
import org.joda.time.DateTime
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.json._
import play.api.libs.ws.{WSAuthScheme, WSClient}
import play.api.{Configuration, Logging}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.xml.Elem

@Singleton
class MavenCentral @Inject() (cache: Cache, memcache: Memcache, wsClient: WSClient, actorSystem: ActorSystem, configuration: Configuration, webJarsFileService: WebJarsFileService)(classic: Classic, bower: Bower, bowerGitHub: BowerGitHub, npm: NPM) (implicit ec: ExecutionContext) extends Logging {

  lazy val webJarFetcher: ActorRef = actorSystem.actorOf(Props[WebJarFetcher]())

  val allWebJarTypes = Set(classic, bower, bowerGitHub, npm)

  implicit val webJarVersionReads = Json.reads[WebJarVersion]
  implicit val webJarVersionWrites = Json.writes[WebJarVersion]

  implicit val webJarReads = Json.reads[WebJar]
  implicit val webJarWrites = Json.writes[WebJar]

  implicit val transcoderInt = new IntegerTranscoder().asInstanceOf[Transcoder[Int]]
  implicit val transcoderElem = new SerializingTranscoder().asInstanceOf[Transcoder[Elem]]

  lazy val ossUsername = configuration.get[String]("oss.username")
  lazy val ossPassword = configuration.get[String]("oss.password")
  lazy val ossProject = configuration.get[String]("oss.project")
  lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)

  lazy val rowLimit = configuration.get[Int]("mavencentral.row-limit")
  lazy val searchUrl = configuration.get[String]("mavencentral.search-url")

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
        Future.successful(s"https://github.com/webjars/$artifactId")
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
        (artifactId, s"https://github.com/webjars/$artifactId")
    }
  }

  def webJarsFromJson(webJarType: WebJarType)(json: JsValue): Future[List[WebJar]] = {

    val allVersions = (json \ "response" \ "docs").as[List[JsObject]].map { jsObject =>
      ((jsObject \ "g").as[String], (jsObject \ "a").as[String], (jsObject \ "v").as[String])
    }

    if (allVersions.size >= rowLimit) {
      logger.error(s"Retrieved max WebJar rows: ${allVersions.size}")
    }
    else {
      logger.info(s"Retrieved ${allVersions.size} WebJars")
    }

    // group by the artifactId
    val artifactsAndVersions = allVersions.groupBy {
      case (groupId, artifactId, _) => groupId -> artifactId
    }.view.filterKeys {
      case (_, artifactId) => !artifactId.startsWith("webjars-")
    } mapValues { versions =>
      versions.map {
        case (_, _, version) => version
      }
    }

    // partition and batch

    def fetchWebJarVersions(artifactAndVersions: ((String, String), List[String])): ((String, String), Future[List[WebJarVersion]]) = {
      val ((groupId, artifactId), versions) = artifactAndVersions
      val versionsFuture = Future.sequence {
        versions.map { version =>
          val cacheKey = s"numfiles-$groupId-$artifactId-$version"
          memcache.getWithMiss[Int](cacheKey) {
            webJarsFileService.getNumFiles(groupId, artifactId, version)
          } map { numFiles =>
            Some(WebJarVersion(version, numFiles))
          } recover {
            case _: FileNotFoundException => None
          }
        }
      } map { webJarVersions =>
        webJarVersions.flatten.sorted.reverse
      }

      (groupId -> artifactId) -> versionsFuture
    }

    def processBatch(resultsFuture: Future[Map[(String, String), List[WebJarVersion]]], batch: Map[(String, String), List[String]]): Future[Map[(String, String), List[WebJarVersion]]] = {
      resultsFuture.flatMap { results =>
        val batchFutures: Map[(String, String), Future[List[WebJarVersion]]] = batch.map(fetchWebJarVersions)

        val batchFuture: Future[Map[(String, String), List[WebJarVersion]]] = Future.traverse(batchFutures.iterator) {
          case ((groupId, artifactId), futureVersions) =>
            futureVersions.map(((groupId, artifactId), _))
        }.map(_.toMap)

        batchFuture.map { batchResult =>
          results ++ batchResult
        }
      }
    }

    // batch size = 100
    val artifactsWithWebJarVersionsFuture: Future[Map[(String, String), List[WebJarVersion]]] = artifactsAndVersions.toMap.grouped(100).foldLeft(Future.successful(Map.empty[(String, String), List[WebJarVersion]]))(processBatch)

    val webJarsFuture: Future[List[WebJar]] = artifactsWithWebJarVersionsFuture.flatMap { artifactsWithWebJarVersions =>
      Future.sequence {
        artifactsWithWebJarVersions.flatMap {
          case ((groupId, artifactId), webJarVersions) =>
            webJarVersions.map(_.number).headOption.map { latestVersion =>
              fetchWebJarNameAndUrl(groupId, artifactId, latestVersion).map {
                case (name, url) =>
                  WebJar(WebJarType.toString(webJarType), groupId, artifactId, name, url, webJarVersions)
              }
            }
        }
      }
    } map { webJars =>
      webJars.toList.sortWith(_.name.toLowerCase < _.name.toLowerCase)
    }

    webJarsFuture
  }

  def fetchWebJarsJson(webJarType: WebJarType): Future[JsValue] = {
    val params = Map(
      "q" -> s"""g:${webJarType.groupIdQuery} AND p:jar""",
      "core" -> "gav",
      "rows" -> rowLimit.toString,
      "wt" -> "json"
    )

    wsClient.url(searchUrl).withQueryStringParameters(params.toSeq: _*).get().flatMap { response =>
      Future.fromTry(Try(response.json)).recoverWith {
        case _ => Future.failed(new MavenCentral.UnavailableException(response.body))
      }
    }
  }

  def fetchWebJars(webJarType: WebJarType): Future[List[WebJar]] = {
    logger.info(s"Getting ${webJarType.name} WebJars")

    fetchWebJarsJson(webJarType).flatMap(webJarsFromJson(webJarType))
  }

  def webJars(webJarType: WebJarType): Future[List[WebJar]] = {
    cache.get[List[WebJar]](webJarType.toString, 1.hour) {
      actorSystem.actorSelection("user/" + webJarType.toString).resolveOne(1.second).flatMap { _ =>
        // in-flight request exists
        Future.failed(new MavenCentral.ExistingWebJarRequestException(webJarType.toString))
      } recoverWith {
        // no request so make one
        case _: ActorNotFound =>
          implicit val timeout: Timeout = Timeout(10.minutes)

          val webJarFetcherTry = Future.fromTry(Try(actorSystem.actorOf(Props(classOf[WebJarFetcher], this, ec), webJarType.toString))).recoverWith {
            case _: InvalidActorNameException => Future.failed(new MavenCentral.ExistingWebJarRequestException(webJarType.toString))
          }

          webJarFetcherTry.flatMap { webJarFetcher =>
            val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars(webJarType)).mapTo[List[WebJar]]

            fetchWebJarsFuture.onComplete(_ => actorSystem.stop(webJarFetcher))

            fetchWebJarsFuture
          }
      }
    }
  }

  def webJars: Future[List[WebJar]] = {
    val allWebJarsFutures = allWebJarTypes.map(webJars)
    Future.foldLeft(allWebJarsFutures)(List.empty[WebJar])(_ ++ _)
  }

  def fetchPom(groupId: String, artifactId: String, version: String, maybeUrlPrefix: Option[String] = None): Future[Elem] = {
    val groupIdPath = groupId.replace(".", "/")
    val urlPrefix = maybeUrlPrefix.getOrElse("https://repo1.maven.org/maven2")
    val url = s"$urlPrefix/$groupIdPath/$artifactId/$version/$artifactId-$version.pom"
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

  def getPom(groupId: String, artifactId: String, version: String): Future[Elem] = {
    val cacheKey = s"pom-$groupId-$artifactId-$version"
    memcache.getWithMiss[Elem](cacheKey) {
      fetchPom(groupId, artifactId, version)
    }
  }

  def groupIds(webJarType: WebJarType): Future[Set[String]] = {
    fetchWebJarsJson(webJarType).map { json =>
      (json \ "response" \ "docs").as[Seq[JsObject]].map(_.\("g").as[String]).toSet
    }
  }

  def getStats(groupIdQuery: String, dateTime: DateTime): Future[Seq[(String, String, Int)]] = {
    val queryString = Seq(
      "p" -> ossProject,
      "g" -> groupIdQuery,
      "t" -> "raw",
      "from" -> dateTime.toString("yyyyMM"),
      "nom" -> "1"
    )

    val statsFuture = wsClient.url("https://oss.sonatype.org/service/local/stats/slices")
      .withHttpHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON)
      .withQueryStringParameters(queryString: _*)
      .withAuth(ossUsername, ossPassword, WSAuthScheme.BASIC)
      .get()

    statsFuture.flatMap { response =>
      response.status match {
        case Status.OK =>

          val groupId = (response.json \ "data" \ "groupId").as[String]
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
            Future.failed(new MavenCentral.EmptyStatsException("Stats were empty"))
          }
        case Status.UNAUTHORIZED =>
          Future.failed(UnauthorizedError("Invalid credentials"))
        case _ =>
          Future.failed(new MavenCentral.UnavailableException(response.body))
      }
    }
  }

  def getStats(webJarType: WebJarType, dateTime: DateTime): Future[Seq[(String, String, Int)]] = {
    if (webJarType.groupIdQuery.endsWith("*")) {
      groupIds(webJarType).flatMap { groupIds =>
        val futures = groupIds.map { groupId =>
          getStats(groupId, dateTime).recover {
            case _: MavenCentral.EmptyStatsException => Seq.empty[(String, String, Int)]
            case _: UnauthorizedError => Seq.empty[(String, String, Int)]
            case _: MavenCentral.UnavailableException => Seq.empty[(String, String, Int)]
          }
        }
        Future.reduceLeft(futures)(_ ++ _)
      }
    }
    else {
      getStats(webJarType.groupIdQuery, dateTime)
    }
  }

  def getStats(dateTime: DateTime): Future[Seq[(String, String, Int)]] = {
    val allStatsFutures = allWebJarTypes.map { webJarType => getStats(webJarType, dateTime) }
    Future.reduceLeft(allStatsFutures)(_ ++ _)
  }

  def mostDownloaded(webJarType: WebJarType, dateTime: DateTime, num: Int): Future[Seq[(String, String, Int)]] = {
    getStats(webJarType, dateTime).map(_.take(num))
  }

  def mostDownloaded(dateTime: DateTime, num: Int): Future[Seq[(String, String, Int)]] = {
    val allMostDownloadedFutures = allWebJarTypes.map { webJarType => mostDownloaded(webJarType, dateTime, num) }
    Future.foldLeft(allMostDownloadedFutures)(Seq.empty[(String, String, Int)])(_ ++ _)
  }

}


object MavenCentral {
  class UnavailableException(msg: String) extends RuntimeException(msg)
  class ExistingWebJarRequestException(groupId: String) extends RuntimeException(s"There is an existing WebJar request for $groupId")
  class EmptyStatsException(msg: String) extends RuntimeException(msg)
}
