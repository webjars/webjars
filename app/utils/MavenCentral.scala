package utils

import java.io.FileNotFoundException
import actors.{FetchWebJars, WebJarFetcher}
import akka.actor._
import akka.pattern.{after, ask}
import akka.util.Timeout

import javax.inject.{Inject, Singleton}
import models.{WebJar, WebJarType, WebJarVersion}
import net.spy.memcached.transcoders.{IntegerTranscoder, SerializingTranscoder, Transcoder}
import org.joda.time.DateTime
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws.{WSAuthScheme, WSClient, WSRequest, WSResponse}
import play.api.{Configuration, Environment, Logging, Mode}
import utils.MavenCentral.EmptyStatsException

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

@Singleton
class MavenCentral @Inject() (cache: Cache, memcache: Memcache, wsClient: WSClient, configuration: Configuration, webJarsFileService: WebJarsFileService)(classic: Classic, bower: Bower, bowerGitHub: BowerGitHub, npm: NPM, environment: Environment) (implicit ec: ExecutionContext, actorSystem: ActorSystem) extends Logging {

  lazy val webJarFetcher: ActorRef = actorSystem.actorOf(Props[WebJarFetcher]())

  val allWebJarTypes = Set(classic, bower, bowerGitHub, npm)

  implicit val webJarVersionReads = Json.reads[WebJarVersion]
  implicit val webJarVersionWrites = Json.writes[WebJarVersion]

  implicit val webJarReads = Json.reads[WebJar]
  implicit val webJarWrites = Json.writes[WebJar]

  implicit val transcoderInt = new IntegerTranscoder().asInstanceOf[Transcoder[Int]]
  implicit val transcoderElem = new SerializingTranscoder().asInstanceOf[Transcoder[Elem]]

  lazy val maybeOssUsername = configuration.getOptional[String]("oss.username")
  lazy val maybeOssPassword = configuration.getOptional[String]("oss.password")
  lazy val maybeOssStagingProfileId = configuration.getOptional[String]("oss.staging-profile")
  lazy val ossProject = configuration.get[String]("oss.project")
  lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)

  lazy val rowLimit = configuration.get[Int]("mavencentral.row-limit")
  lazy val searchUrl = configuration.get[String]("mavencentral.search-url")

  def withOssCredentials[T](f: (String, String) => Future[T]): Future[T] = {
    val maybeUsernameAndPassword = for {
      ossUsername <- maybeOssUsername
      ossPassword <- maybeOssPassword
    } yield (ossUsername, ossPassword)

    maybeUsernameAndPassword.fold(Future.failed[T](new IllegalArgumentException("oss.username or oss.password not set"))) { case (ossUsername, ossPassword) =>
      f(ossUsername, ossPassword)
    }
  }

  def fetchWebJarNameAndUrl(groupId: String, artifactId: String, version: String): Future[(String, String)] = {
    getPom(groupId, artifactId, version).flatMap { xml =>
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

    if ((allVersions.size >= rowLimit) && (environment.mode == Mode.Prod)) {
      logger.error(s"Retrieved max ${webJarType.name} WebJar rows: ${allVersions.size}")
    }
    else {
      logger.info(s"Retrieved ${allVersions.size} ${webJarType.name} WebJars")
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

  def fetchWebJars(webJarType: WebJarType, dateTime: DateTime = DateTime.now().minusMonths(1)): Future[List[WebJar]] = {
    logger.info(s"Getting ${webJarType.name} WebJars")

    fetchWebJarsJson(webJarType).flatMap(webJarsFromJson(webJarType)).flatMap { webJars =>
      val statsFuture = getStats(webJarType, dateTime).recoverWith {
        case _: EmptyStatsException => getStats(webJarType, dateTime.minusMonths(1))
      } recover {
        // if the stats can't be fetched, continue without them
        case e: Exception =>
          logger.error(s"Could not get stats for $webJarType", e)
          Map.empty[(String, String), Int]
      }

      statsFuture.map { stats =>
        webJars.sortBy { webJar =>
          stats.getOrElse((webJar.groupId, webJar.artifactId), 0)
        } (Ordering[Int].reverse)
      }
    }
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

  def getStats(groupIdQuery: String, dateTime: DateTime): Future[Map[(String, String), Int]] = {
    val queryString = Seq(
      "p" -> ossProject,
      "g" -> groupIdQuery,
      "t" -> "raw",
      "from" -> dateTime.toString("yyyyMM"),
      "nom" -> "1"
    )


    withOssCredentials { (ossUsername, ossPassword) =>
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
                (groupId, name) -> count
              }.toMap

              Future.successful(webJarCounts)
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
  }

  def getStats(webJarType: WebJarType, dateTime: DateTime): Future[Map[(String, String), Int]] = {
    if (webJarType.groupIdQuery.endsWith("*")) {
      groupIds(webJarType).flatMap { groupIds =>
        val futures = groupIds.map { groupId =>
          getStats(groupId, dateTime).recover {
            case _: MavenCentral.EmptyStatsException => Map.empty[(String, String), Int]
            case _: UnauthorizedError => Map.empty[(String, String), Int]
            case _: MavenCentral.UnavailableException => Map.empty[(String, String), Int]
          }
        }
        Future.reduceLeft(futures)(_ ++ _)
      }
    }
    else {
      getStats(webJarType.groupIdQuery, dateTime)
    }
  }

  /*
  private def ossStagingT[T](path: String)(f: WSRequest => Future[WSResponse])(implicit reads: Reads[T]): Future[T] = {
    ossStaging(path)(f).flatMap { json =>
      Future.fromTry(JsResult.toTry(json.validate[T](reads)))
    }
  }

   */

  private def stagingRepo(stagedRepo: StagedRepo): Future[JsValue] = {
    withOssCredentials { (username, password) =>
      val url = s"https://oss.sonatype.org/service/local/staging/repository/${stagedRepo.id}"
      wsClient.url(url).withAuth(username, password, WSAuthScheme.BASIC).withHttpHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).get().flatMap { response =>
        if (response.status == Status.OK) {
          Future.fromTry {
            Try {
              response.json
            }
          }
        }
        else {
          Future.failed(ServerError(response.body, response.status))
        }
      }
    }
  }

  // kudos: https://nami.me/2015/01/20/scala-futures-with-timeout/
  implicit class FutureExtensions[T](f: Future[T]) {
    def withTimeout(duration: FiniteDuration)(implicit system: ActorSystem): Future[T] = {
      val max = after(duration, system.scheduler)(Future.failed(new TimeoutException(s"Operation did not complete in $duration")))
      Future.firstCompletedOf(Seq(f, max))
    }
  }

  val maxwait = 10.minutes
  val poll = 5.seconds

  // todo: errors
  private def stagingWait(stagedRepo: StagedRepo): Future[JsValue] = {
    def tryAgain(): Future[JsValue] = {
      stagingRepo(stagedRepo).flatMap { json =>
        if ((json \ "transitioning").as[Boolean]) {
          after(poll, actorSystem.scheduler)(tryAgain())
        }
        else {
          Future.successful(json)
        }
      }
    }

    tryAgain().withTimeout(maxwait)
  }

  private def ossStagingProfiles(path: String, responseCode: Int)(f: WSRequest => Future[WSResponse]): Future[Option[JsValue]] = {
    maybeOssStagingProfileId.fold(Future.failed[Option[JsValue]](new Exception("Could not get staging profile id"))) { stagingProfileId =>
      withOssCredentials { (username, password) =>
        val url = s"https://oss.sonatype.org/service/local/staging/profiles/$stagingProfileId/$path"
        val req = wsClient.url(url).withAuth(username, password, WSAuthScheme.BASIC).withHttpHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON)
        f(req).flatMap { response =>
          val result = if (response.status == responseCode && response.contentType.startsWith(MimeTypes.JSON)) {
            Try {
              Some(response.json)
            }
          }
          else if (response.status == responseCode && response.bodyAsBytes.isEmpty) {
            Success(None)
          }
          else {
            Failure(ServerError(response.body, response.status))
          }

          Future.fromTry(result)
        }
      }
    }
  }

  /*
  def stagingRepositoryProfiles(): Future[JsValue] = {
    ossStagingProfiles("profile_repositories")(_.get())
  }
   */

  case class StagedRepo(id: String, description: String)

  object StagedRepo {
    implicit val jsonReads: Reads[StagedRepo] = (
      (__ \ "stagedRepositoryId").read[String] ~
      (__ \ "description").read[String]
    ).apply(StagedRepo(_, _))
  }

  def createStaging(description: String): Future[StagedRepo] = {
    ossStagingProfiles("start", Status.CREATED) { req =>
      val body = Json.obj(
        "data" -> Json.obj(
          "description" -> description
        )
      )

      req.post(body)
    } collect {
      case Some(jsValue) => jsValue
    } flatMap { jsValue =>
      (jsValue \ "data").asOpt[StagedRepo].fold(Future.failed[StagedRepo](new Exception("Could not parse response")))(Future.successful)
    }
  }

  def closeStaging(stagedRepo: StagedRepo, description: String): Future[Unit] = {
    ossStagingProfiles("finish", Status.CREATED) { req =>
      val body = Json.obj(
        "data" -> Json.obj(
          "stagedRepositoryId" -> stagedRepo.id,
          "targetRepositoryId" -> ossProject,
          "description" -> description,
        )
      )

      req.post(body)
    } collect {
      case None => ()
    } flatMap { _ =>
      stagingWait(stagedRepo).map(_ => ())
    }
  }

  def promoteStaging(stagedRepo: StagedRepo, description: String): Future[Unit] = {
    ossStagingProfiles("promote", Status.CREATED) { req =>
      val body = Json.obj(
        "data" -> Json.obj(
          "stagedRepositoryId" -> stagedRepo.id,
          "targetRepositoryId" -> ossProject,
          "description" -> description,
        )
      )

      req.post(body)
    } collect {
      case None => ()
    } flatMap { _ =>
      stagingWait(stagedRepo).map(_ => ())
    }
  }

}


object MavenCentral {
  class UnavailableException(msg: String) extends RuntimeException(msg)
  class ExistingWebJarRequestException(groupId: String) extends RuntimeException(s"There is an existing WebJar request for $groupId")
  class EmptyStatsException(msg: String) extends RuntimeException(msg)
}