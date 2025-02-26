package utils

import actors.{FetchWebJars, WebJarFetcher}
import com.google.inject.ImplementedBy
import com.roundeights.hasher.Implicits._
import models.{WebJar, WebJarVersion}
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
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

@ImplementedBy(classOf[MavenCentralLive])
trait MavenCentral {
  import MavenCentral._

  def maybeOssUsername(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.username").filter(_.nonEmpty)
  def maybeOssPassword(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.password").filter(_.nonEmpty)
  def maybeOssGpgKey(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-key").filter(_.nonEmpty)
  def maybeOssGpgPass(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-pass").filter(_.nonEmpty)

  def fetchWebJars(groupId: GroupId): Future[Set[WebJar]]
  def fetchPom(gav: GAV, maybeUrlPrefix: Option[String] = None): Future[Elem]
  def webJars(groupId: GroupId): Future[List[WebJar]]
  def webJarsSorted(maybeGroupdId: Option[GroupId] = None, dateTime: LocalDateTime = LocalDateTime.now().minusMonths(1)): Future[List[WebJar]]
  def getStats(groupId: GroupId, dateTime: LocalDateTime): Future[Map[(String, String), Int]]

  def authToken(): Future[(String, String)]

  def asc(toSign: Array[Byte]): Option[String]
  def createStaging(description: String): Future[StagedRepo]
  def uploadStaging(stagedRepo: StagedRepo, gav: GAV, pom: String, jar: Array[Byte]): Future[Unit]
  def closeStaging(stagedRepo: StagedRepo, description: String): Future[Unit]
  def promoteStaging(stagedRepo: StagedRepo, description: String): Future[Unit]
  def dropStaging(stagedRepo: StagedRepo, description: String): Future[Unit]
}

@Singleton
class MavenCentralLive @Inject() (memcache: Memcache, wsClient: WSClient, configuration: Configuration, webJarsFileService: WebJarsFileService, environment: Environment)
                                 (allDeployables: AllDeployables)
                                 (implicit ec: ExecutionContext, actorSystem: ActorSystem) extends MavenCentral with Logging {
  import MavenCentral._

  private implicit val transcoderOptionalInt: Transcoder[Option[Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Option[Int]]]
  private implicit val transcoderNameUrl: Transcoder[(String, String)] = new SerializingTranscoder().asInstanceOf[Transcoder[(String, String)]]
  private implicit val transcoderStats: Transcoder[Map[(String, String), Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Map[(String, String), Int]]]

  private lazy val maybeLimit = configuration.getOptional[Int]("mavencentral.limit").orElse(Option.when(environment.mode == Mode.Dev)(5))

  private lazy val maybeOssStagingProfileId = configuration.getOptional[String]("oss.staging-profile")
  private lazy val ossProject = configuration.get[String]("oss.project")
  private lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)


  def withOssCredentials[T](f: (String, String) => Future[T]): Future[T] = {
    val maybeUsernameAndPassword = for {
      ossUsername <- maybeOssUsername(configuration)
      ossPassword <- maybeOssPassword(configuration)
    } yield (ossUsername, ossPassword)

    maybeUsernameAndPassword.fold(Future.failed[T](new NoAccess("oss.username or oss.password not set"))) { case (ossUsername, ossPassword) =>
      f(ossUsername, ossPassword)
    }
  }

  // docs: https://support.sonatype.com/hc/en-us/articles/213465878-How-to-retrieve-a-user-token-from-Nexus-Repository-using-REST
  override def authToken(): Future[(String, String)] = {
    withOssCredentials { (ossUsername, ossPassword) =>
      val j = Json.obj(
        "u" -> Base64.getEncoder.encodeToString(ossUsername.getBytes),
        "p" -> Base64.getEncoder.encodeToString(ossPassword.getBytes)
      )

      val ticket = wsClient.url("https://oss.sonatype.org/service/siesta/wonderland/authenticate")
        .withHttpHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON)
        .withAuth(ossUsername, ossPassword, WSAuthScheme.BASIC)
        .post(j)

      ticket.flatMap { ticketResponse =>
        if (ticketResponse.status == Status.OK) {
          (ticketResponse.json \ "t").asOpt[String].fold[Future[(String, String)]](
            Future.failed(new Error("Could not get auth ticket"))
          ) { ticket =>
            wsClient.url("https://oss.sonatype.org/service/siesta/usertoken/current")
              .withHttpHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON, "X-NX-AuthTicket" -> ticket)
              .withAuth(ossUsername, ossPassword, WSAuthScheme.BASIC)
              .get()
              .flatMap { response =>
                if (response.status == Status.OK) {
                  val maybeNameCode = (response.json \ "nameCode").asOpt[String]
                  val maybePassCode = (response.json \ "passCode").asOpt[String]

                  val maybeCredentials = for {
                    nameCode <- maybeNameCode
                    passCode <- maybePassCode
                  } yield (nameCode, passCode)

                  maybeCredentials.fold[Future[(String, String)]](Future.failed(new Error("Could not get auth token")))(Future.successful)
                }
                else {
                  Future.failed(new Error("Could not get auth token"))
                }
              }
          }
        }
        else {
          Future.failed(new Error(ticketResponse.statusText))
        }
      }
    }
  }

  lazy val authTokenFuture: Future[(String, String)] = authToken()

  def withOssDeployCredentials[T](f: (String, String) => Future[T]): Future[T] = {
    authTokenFuture.flatMap { case (username, password) =>
      f(username, password)
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

  def fetchGAVs(groupId: GroupId): Future[Set[GAV]] = {
    // 200 seems to be the max for search.maven.org
    val maxPageSize = 200

    val pageSize = maybeLimit.map(Math.min(maxPageSize, _)).getOrElse(maxPageSize)

    val req = wsClient.url("https://search.maven.org/solrsearch/select")
      .withQueryStringParameters("q" -> s"g:$groupId", "wt" -> "json")

    req.addQueryStringParameters("rows" -> "0").get().flatMap { numResponse =>
      val numFound = (numResponse.json \ "response" \ "numFound").as[Int]
      val toFetch = maybeLimit.getOrElse(numFound)

      val pages = (0 until toFetch).by(pageSize).map { start =>
        req.addQueryStringParameters("core" -> "gav", "rows" -> pageSize.toString, "start" -> start.toString).get().map { response =>
          (response.json \ "response" \ "docs").as[Seq[JsObject]].map { doc =>
            val artifactId = (doc \ "a").as[String]
            val version = (doc \ "v").as[String]
            GAV(groupId, artifactId, version)
          }
        }
      }

      Future.reduceLeft(pages)(_ ++ _).map { gavs =>
        // ignore webjar libraries
        gavs.filterNot(_.artifactId.startsWith("webjars-")).toSet
      }
    }
  }

  def gavsToWebJarVersion(gavs: Set[GAV]): Future[Set[WebJarVersion]] = {
    Future.sequence {
      gavs.map { gav =>
        getNumFiles(gav).map { maybeNumFiles =>
          maybeNumFiles.map { numFiles =>
            WebJarVersion(gav.version, numFiles)
          }
        }
      }
    }.map(_.flatten)
  }

  def fetchWebJars(groupId: GroupId): Future[Set[WebJar]] = {
    logger.info(s"Getting $groupId WebJars")

    fetchGAVs(groupId).flatMap { gavs =>
      Future.sequence {
        gavs.groupBy { gav =>
          gav.groupId -> gav.artifactId
        }.map { case ((groupId, artifactId), artifactGavs) =>
          gavsToWebJarVersion(artifactGavs).flatMap { versions =>
            val sorted = versions.toSeq.sortBy(_.number)(VersionStringOrdering.compare).reverse

            getWebJarNameAndUrl(GAV(groupId, artifactId, sorted.head.number)).map { case (name, url) =>
              WebJar(groupId, artifactId, name, url, sorted)
            }
          }
        }.toSet
      }
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

  def webJarsSorted(maybeGroupId: Option[GroupId], dateTime: LocalDateTime): Future[List[WebJar]] = {
    val webJarTypes = maybeGroupId.fold(allDeployables.groupIds())(Set(_))
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

  def getStats(groupId: GroupId, dateTime: LocalDateTime): Future[Map[(String, String), Int]] = {
    memcache.getWithMiss(s"stats-$groupId", Expiration.In(1.day)) {
      fetchStats(groupId, dateTime).recover {
        case e: NoAccess =>
          logger.error("Maven Central Error", e)
          Map.empty[(String, String), Int]
      }
    }
  }

  private def stagingRepo(stagedRepo: StagedRepo): Future[JsValue] = {
    withOssDeployCredentials { (username, password) =>
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
      withOssDeployCredentials { (username, password) =>
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
    stagingTransitionWithWait("finish", stagedRepo, description).recoverWith {
      case _ => dropStaging(stagedRepo, description)
    }
  }

  def promoteStaging(stagedRepo: StagedRepo, description: String): Future[Unit] = {
    if (!disableDeploy) {
      stagingTransitionWithWait("promote", stagedRepo, description).recoverWith {
        case _ => dropStaging(stagedRepo, description)
      }
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

      val sGen = new PGPSignatureGenerator(signerBuilder, secretKeyRing.getPublicKey())
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
  class ExistingWebJarRequestException(groupId: GroupId) extends RuntimeException(s"There is an existing WebJar request for $groupId")
  class EmptyStatsException(msg: String) extends RuntimeException(msg)
  class NoAccess(msg: String) extends RuntimeException(msg)

  type GroupId = String
  type ArtifactId = String

  case class GAV(groupId: GroupId, artifactId: ArtifactId, version: String) {
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
