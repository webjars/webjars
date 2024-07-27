package utils

import actors.{FetchWebJars, WebJarFetcher}
import com.google.inject.ImplementedBy
import com.roundeights.hasher.Implicits._
import models.{WebJar, WebJarType, WebJarVersion}
import net.spy.memcached.transcoders.{IntegerTranscoder, SerializingTranscoder, Transcoder}
import org.apache.pekko.actor._
import org.apache.pekko.pattern.{after, ask}
import org.apache.pekko.util.Timeout
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.{BodyWritable, WSAuthScheme, WSClient, WSRequest, WSResponse}
import play.api.{Configuration, Environment, Logging, Mode}
import utils.Memcache.Expiration

import java.io.{ByteArrayOutputStream, FileNotFoundException}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Base64
import java.util.concurrent.TimeoutException
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.{Failure, Random, Success, Try}
import scala.xml.Elem

@ImplementedBy(classOf[MavenCentralLive])
trait MavenCentral {
  import MavenCentral._

  def maybeOssUsername(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.username").filter(_.nonEmpty)
  def maybeOssPassword(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.password").filter(_.nonEmpty)
  def maybeOssDeployUsername(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.deploy.username").filter(_.nonEmpty)
  def maybeOssDeployPassword(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.deploy.password").filter(_.nonEmpty)
  def maybeOssGpgKey(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-key").filter(_.nonEmpty)
  def maybeOssGpgPass(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-pass").filter(_.nonEmpty)

  def artifactIds(groupId: String): Future[Set[String]]
  def fetchWebJars(webJarType: WebJarType): Future[Set[WebJar]]
  def fetchPom(gav: GAV, maybeUrlPrefix: Option[String] = None): Future[Elem]
  def webJars(webJarType: WebJarType): Future[List[WebJar]]
  def webJarsSorted(maybeWebJarType: Option[WebJarType] = None, dateTime: LocalDateTime = LocalDateTime.now().minusMonths(1)): Future[List[WebJar]]
  def getStats(webJarType: WebJarType, dateTime: LocalDateTime): Future[Map[(String, String), Int]]

  def asc(toSign: Array[Byte]): Option[String]
  def createStaging(description: String): Future[StagedRepo]
  def uploadStaging(stagedRepo: StagedRepo, gav: GAV, pom: String, jar: Array[Byte]): Future[Unit]
  def closeStaging(stagedRepo: StagedRepo, description: String): Future[Unit]
  def promoteStaging(stagedRepo: StagedRepo, description: String): Future[Unit]
  def dropStaging(stagedRepo: StagedRepo, description: String): Future[Unit]
}

@Singleton
class MavenCentralLive @Inject() (memcache: Memcache, wsClient: WSClient, configuration: Configuration, webJarsFileService: WebJarsFileService, environment: Environment)
                                 (classic: Classic, bower: Bower, bowerGitHub: BowerGitHub, npm: NPM)
                                 (implicit ec: ExecutionContext, actorSystem: ActorSystem) extends MavenCentral with Logging {
  import MavenCentral._

  val allWebJarTypes: Set[WebJarType] = Set(classic, bower, bowerGitHub, npm)

  private implicit val transcoderInt: Transcoder[Int] = new IntegerTranscoder().asInstanceOf[Transcoder[Int]]
  private implicit val transcoderNameUrl: Transcoder[(String, String)] = new SerializingTranscoder().asInstanceOf[Transcoder[(String, String)]]
  private implicit val transcoderStats: Transcoder[Map[(String, String), Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Map[(String, String), Int]]]
  private implicit val transcoderVersions: Transcoder[List[(String, Int)]] = new SerializingTranscoder().asInstanceOf[Transcoder[List[(String, Int)]]]
  private implicit val transcoderArtifactIds: Transcoder[Set[String]] = new SerializingTranscoder().asInstanceOf[Transcoder[Set[String]]]

  private lazy val maybeLimit = configuration.getOptional[Int]("mavencentral.limit").orElse(Option.when(environment.mode == Mode.Dev)(5))

  private lazy val maybeOssStagingProfileId = configuration.getOptional[String]("oss.staging-profile")
  private lazy val ossProject = configuration.get[String]("oss.project")
  private lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)


  def withOssCredentials[T](f: (String, String) => Future[T]): Future[T] = {
    val maybeUsernameAndPassword = for {
      ossUsername <- maybeOssUsername(configuration)
      ossPassword <- maybeOssPassword(configuration)
    } yield (ossUsername, ossPassword)

    maybeUsernameAndPassword.fold(Future.failed[T](new IllegalArgumentException("oss.username or oss.password not set"))) { case (ossUsername, ossPassword) =>
      f(ossUsername, ossPassword)
    }
  }

  def withOssDeployCredentials[T](f: (String, String) => Future[T]): Future[T] = {
    val maybeUsernameAndPassword = for {
      ossDeployUsername <- maybeOssDeployUsername(configuration)
      ossDeployPassword <- maybeOssDeployPassword(configuration)
    } yield (ossDeployUsername, ossDeployPassword)

    maybeUsernameAndPassword.fold(Future.failed[T](new IllegalArgumentException("oss.username or oss.password not set"))) { case (ossDeployUsername, ossDeployPassword) =>
      f(ossDeployUsername, ossDeployPassword)
    }
  }

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

  def getWebJarNameAndUrl(gav: GAV): Future[(String, String)] = {
    memcache.getWithMiss[(String, String)](s"name-url-${gav.cacheKey}") {
      fetchWebJarNameAndUrl(gav)
    }
  }

  sealed trait Include
  case object OnlyDated extends Include
  case object OnlyUndated extends Include

  def fetchDirs(url: String, include: Include): Future[Set[String]] = {
    val filenameExtractor: Regex = """.*<a href="([^"]+)".*""".r
    val filenameAndDateExtractor: Regex = """.*<a href="([^"]+)".*</a>\s+(\d+-\d+-\d+\s\d+:\d+)\s+-.*""".r
    wsClient.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful {
            response.body.linesIterator.collect[(String, Option[String])] {
              case filenameAndDateExtractor(name, date) =>
                name.stripSuffix("/") -> Some(date)
              case filenameExtractor(name) if name.endsWith("/") && !name.startsWith("..") =>
                name.stripSuffix("/") -> None
            }.filter { case (_, maybeDate) =>
              include match {
                case OnlyDated => maybeDate.isDefined
                case OnlyUndated => maybeDate.isEmpty
              }
            }.map { case (name, _) =>
              name
            }.toSet
          }
        case _ =>
          Future.failed(new Exception(s"Error fetching $url ${response.status} \n ${response.body}"))
      }
    }
  }

  def groupIds(webJarType: WebJarType): Future[Set[String]] = {
    if (webJarType.groupIdQuery.endsWith(".*")) {
      val groupPath = webJarType.groupIdQuery.stripSuffix(".*").split('.').mkString("", "/", "/")
      val groupUrl = s"https://repo1.maven.org/maven2/$groupPath"

      fetchDirs(groupUrl, OnlyUndated).map { dirs =>
        val groupIds = dirs.map { dir =>
          webJarType.groupIdQuery.replace("*", dir)
        }

        maybeLimit.fold(groupIds)(groupIds.take)
      }
    }
    else {
      Future.successful(Set(webJarType.groupIdQuery))
    }
  }

  def artifactIds(groupId: String): Future[Set[String]] = {
    val groupPath = groupId.split('.').mkString("", "/", "/")
    val groupUrl = s"https://repo1.maven.org/maven2/$groupPath"

    fetchDirs(groupUrl, OnlyUndated).map { dirs =>
      val artifactIds = dirs
        .filterNot(_.startsWith("webjars-"))
        .filterNot(_ == "2.11.2") // workaround because https://repo1.maven.org/maven2/org/webjars/npm/ has 2.11.2 without a date

      // with the limit, sort before taking so the output is more stable
      maybeLimit.fold(artifactIds)(artifactIds.toList.sortBy(_.toLowerCase).take(_).toSet)
    }
  }

  def fetchWebJarVersions(groupId: String, artifactId: String): Future[List[WebJarVersion]] = {
    val jitter = (Random.nextInt(10) + 55).minutes

    memcache.getWithMiss[List[(String, Int)]](s"versions-$groupId-$artifactId", Expiration.In(jitter)) {
      val groupPath = groupId.split('.').mkString("/")
      val url = s"https://repo1.maven.org/maven2/$groupPath/$artifactId"

      fetchDirs(url, OnlyDated).flatMap { versions =>
        Future.sequence {
          versions.map { version =>
            val cacheKey = s"numfiles-$groupId-$artifactId-$version"
            memcache.getWithMiss[Int](cacheKey) {
              webJarsFileService.getNumFiles(groupId, artifactId, version)
            } map { numFiles =>
              Some(version -> numFiles)
            } recover {
              case _: FileNotFoundException => None
            }
          }
        } map { webJarVersions =>
          webJarVersions.flatten.toList.sortBy(_._1)(VersionStringOrdering.compare).reverse
        }
      }
    }.map { versions =>
      versions.map { case (number, downloads) =>
        WebJarVersion(number, downloads)
      }
    }
  }

  def fetchWebJar(webJarType: WebJarType, groupId: String, artifactId: String): Future[WebJar] = {
    fetchWebJarVersions(groupId, artifactId).flatMap { webJarVersions =>
      webJarVersions.map(_.number).headOption.fold {
        Future.failed[WebJar](new Exception(s"Could not fetch $groupId $artifactId because it did not have versions"))
      } { latestVersion =>
        getWebJarNameAndUrl(GAV(groupId, artifactId, latestVersion)).map {
          case (name, url) =>
            WebJar(WebJarType.toString(webJarType), groupId, artifactId, name, url, webJarVersions)
        }
      }
    }
  }

  def fetchBatch(f: Future[Set[WebJar]], webJarType: WebJarType, groupId: String, artifactIds: Set[String], batchSize: Int = 20): Future[Set[WebJar]] = {
    f.flatMap { alreadyFetched =>
      val (doNow, doNext) = artifactIds.splitAt(batchSize)

      if (doNow.isEmpty) {
        Future.successful(alreadyFetched)
      }
      else {
        val newF = Future.sequence {
          doNow.map { artifactId =>
            fetchWebJar(webJarType, groupId, artifactId).map(Some(_)).recover {
              case e =>
                logger.trace(s"Could not fetch $groupId $artifactId", e)
                None
            }
          }
        }.map { newWebJars =>
          alreadyFetched ++ newWebJars.flatten
        }

        fetchBatch(newF, webJarType, groupId, doNext)
      }
    }
  }

  def fetchWebJars(webJarType: WebJarType): Future[Set[WebJar]] = {
    logger.info(s"Getting ${webJarType.name} WebJars")

    groupIds(webJarType).flatMap { groupIds =>
      val futures = groupIds.map { groupId =>
        val jitter = (Random.nextInt(4) + 20).hours

        memcache.getWithMiss(s"artifactIds-$groupId", Expiration.In(jitter)) {
          artifactIds(groupId)
        }.flatMap { artifactIds =>
          fetchBatch(Future.successful(Set.empty), webJarType, groupId, artifactIds)
        }
      }

      Future.reduceLeft(futures)(_ ++ _)
    }
  }

  def webJars(webJarType: WebJarType): Future[List[WebJar]] = {
    actorSystem.actorSelection("user/" + webJarType.toString).resolveOne(1.second).flatMap { _ =>
      // in-flight request exists
      Future.failed(new ExistingWebJarRequestException(webJarType.toString))
    } recoverWith {
      // no request so make one
      case _: ActorNotFound =>
        implicit val timeout: Timeout = Timeout(10.minutes)

        val webJarFetcherTry = Future.fromTry(Try(actorSystem.actorOf(Props(classOf[WebJarFetcher], this, ec), webJarType.toString))).recoverWith {
          case _: InvalidActorNameException => Future.failed(new ExistingWebJarRequestException(webJarType.toString))
        }

        webJarFetcherTry.flatMap { webJarFetcher =>
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars(webJarType)).mapTo[Set[WebJar]]

          fetchWebJarsFuture.onComplete(_ => actorSystem.stop(webJarFetcher))

          fetchWebJarsFuture.map(_.toList.sortBy(_.name.toLowerCase))
        }
    }
  }

  def webJarsSorted(maybeWebJarType: Option[WebJarType], dateTime: LocalDateTime): Future[List[WebJar]] = {
    val webJarTypes = maybeWebJarType.fold(allWebJarTypes)(Set(_))
    val webJarsFutures = webJarTypes.map(webJars)

    val statsFuture = Future.reduceLeft {
      webJarTypes.map { webJarType =>
        getStats(webJarType, dateTime).recoverWith {
          case _: EmptyStatsException => getStats(webJarType, dateTime.minusMonths(1))
        } recover {
          // if the stats can't be fetched, continue without them
          case e: Exception =>
            logger.error(s"Could not get stats for $webJarType", e)
            Map.empty[(String, String), Int]
        }
      }
    }(_ ++ _)

    statsFuture.flatMap { stats =>
      Future.reduceLeft(webJarsFutures)(_ ++ _).map { webJars =>
        val webJarsWithDownloads = webJars.map { webJar =>
          webJar -> stats.getOrElse((webJar.groupId, webJar.artifactId), 0)
        }

        webJarsWithDownloads.sortBy(_._2)(Ordering[Int].reverse).map(_._1)
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

  def fetchStats(groupIdQuery: String, dateTime: LocalDateTime): Future[Map[(String, String), Int]] = {
    val queryString = Seq(
      "p" -> ossProject,
      "g" -> groupIdQuery,
      "t" -> "raw",
      "from" -> dateTime.format(DateTimeFormatter.ofPattern("yyyyMM")),
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
              Future.failed(new EmptyStatsException("Stats were empty"))
            }
          case Status.UNAUTHORIZED =>
            Future.failed(UnauthorizedError("Invalid credentials"))
          case _ =>
            Future.failed(new UnavailableException(response.body))
        }
      }
    }
  }

  def getStats(webJarType: WebJarType, dateTime: LocalDateTime): Future[Map[(String, String), Int]] = {
    memcache.getWithMiss(s"stats-$webJarType", Expiration.In(1.day)) {
      groupIds(webJarType).flatMap { groupIds =>
        val futures = groupIds.map { groupId =>
          fetchStats(groupId, dateTime).recover {
            case _: EmptyStatsException => Map.empty[(String, String), Int]
          }
        }
        Future.reduceLeft(futures)(_ ++ _)
      }
    }
  }

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

  private val maxwait = 10.minutes
  private val poll = 5.seconds

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

  private def stagingProfileReq(path: String, responseCode: Int)(f: WSRequest => Future[WSResponse]): Future[Option[JsValue]] = {
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

  private def stagingTransition(state: String, stagedRepo: StagedRepo, description: String): Future[Unit] = {
    stagingProfileReq(state, Status.CREATED) { req =>
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
    }
  }

  private def stagingTransitionWithWait(state: String, stagedRepo: StagedRepo, description: String): Future[Unit] = {
    stagingTransition(state, stagedRepo, description).flatMap { _ =>
      stagingWait(stagedRepo).map(_ => ())
    }
  }

  def createStaging(description: String): Future[StagedRepo] = {
    stagingProfileReq("start", Status.CREATED) { req =>
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
    stagingTransitionWithWait("finish", stagedRepo, description)
  }

  def promoteStaging(stagedRepo: StagedRepo, description: String): Future[Unit] = {
    if (!disableDeploy) {
      stagingTransitionWithWait("promote", stagedRepo, description)
    }
    else {
      Future.unit
    }
  }

  def dropStaging(stagedRepo: StagedRepo, description: String): Future[Unit] = {
    stagingTransition("drop", stagedRepo, description)
  }

  def asc(toSign: Array[Byte]): Option[String] = {
    maybeOssGpgKey(configuration).map { gpgKey =>
      val keyBytes = Base64.getDecoder.decode(gpgKey)

      val secretKeyRing = new PGPSecretKeyRing(keyBytes, new JcaKeyFingerprintCalculator())

      val pass = maybeOssGpgPass(configuration).getOrElse("").toCharArray

      val privKey = secretKeyRing.getSecretKey.extractPrivateKey(new JcePBESecretKeyDecryptorBuilder().build(pass))

      val signerBuilder = new JcaPGPContentSignerBuilder(secretKeyRing.getPublicKey().getAlgorithm, HashAlgorithmTags.SHA1)

      val sGen = new PGPSignatureGenerator(signerBuilder)
      sGen.init(PGPSignature.BINARY_DOCUMENT, privKey)

      val bos = new ByteArrayOutputStream()
      val armor = new ArmoredOutputStream(bos)
      val bOut = new BCPGOutputStream(armor)

      sGen.update(toSign)
      sGen.generate().encode(bOut)

      bOut.close()
      armor.close()
      bos.close()

      new String(bos.toByteArray)
    }
  }

  def uploadStaging(stagedRepo: StagedRepo, gav: GAV, pom: String, jar: Array[Byte]): Future[Unit] = {
    withOssDeployCredentials { (username, password) =>
      val fileUrl = s"https://oss.sonatype.org/service/local/staging/deployByRepositoryId/${stagedRepo.id}/${gav.path}"

      def upload[A](url: String, content: A)(implicit bodyWritable: BodyWritable[A]) = {
        wsClient.url(url).withAuth(username, password, WSAuthScheme.BASIC).put(content).collect(_.status == Status.CREATED)
      }

      val maybeAscs = for {
        pomAsc <- asc(pom.getBytes)
        jarAsc <- asc(jar)
      } yield (pomAsc, jarAsc)

      maybeAscs.fold(Future.failed[Unit](new Exception("Could not create ascs"))) { case (pomAsc, jarAsc) =>
        Future.sequence(
          Seq(
            upload(fileUrl + ".pom", pom),
            upload(fileUrl + ".pom.sha1", pom.sha1.hex),
            upload(fileUrl + ".pom.md5", pom.md5.hex),
            upload(fileUrl + ".pom.asc", pomAsc),

            upload(fileUrl + ".jar", jar),
            upload(fileUrl + ".jar.sha1", jar.sha1.hex),
            upload(fileUrl + ".jar.md5", jar.md5.hex),
            upload(fileUrl + ".jar.asc", jarAsc),
          )
        ).map(_ => ())
      }
    }
  }

}


object MavenCentral {
  class UnavailableException(msg: String) extends RuntimeException(msg)
  class ExistingWebJarRequestException(groupId: String) extends RuntimeException(s"There is an existing WebJar request for $groupId")
  class EmptyStatsException(msg: String) extends RuntimeException(msg)

  case class GAV(groupId: String, artifactId: String, version: String) {
    private lazy val groupIdPath = groupId.replace(".", "/")
    lazy val path = s"$groupIdPath/$artifactId/$version/$artifactId-$version"
    lazy val cacheKey = s"$groupId-$artifactId-$version"

    override def toString: String = s"$groupId:$artifactId:$version"
  }

  case class StagedRepo(id: String, description: String)

  object StagedRepo {
    implicit val jsonReads: Reads[StagedRepo] = (
      (__ \ "stagedRepositoryId").read[String] ~
        (__ \ "description").read[String]
      ).apply(StagedRepo(_, _))
  }
}
