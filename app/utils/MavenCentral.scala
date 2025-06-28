package utils

import actors.{FetchWebJars, WebJarFetcher}
import com.google.inject.ImplementedBy
import com.lumidion.sonatype.central.client.core.PublishingType.USER_MANAGED
import com.lumidion.sonatype.central.client.core._
import com.lumidion.sonatype.central.client.requests.SyncSonatypeClient
import com.roundeights.hasher.Implicits._
import models.{WebJar, WebJarVersion}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.spy.memcached.transcoders.{SerializingTranscoder, Transcoder}
import org.apache.pekko.actor._
import org.apache.pekko.pattern.{after, ask}
import org.apache.pekko.util.Timeout
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.concurrent.Futures
import play.api.libs.concurrent.Futures.FutureOps
import play.api.libs.json._
import play.api.libs.ws.{WSAuthScheme, WSClient}
import play.api.{Configuration, Environment, Logging, Mode}
import utils.Memcache.Expiration

import java.io.{ByteArrayOutputStream, FileNotFoundException}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Base64
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try
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

  def fetchWebJars(groupId: GroupId): Future[Set[WebJar]]
  def fetchPom(gav: GAV, maybeUrlPrefix: Option[String] = None): Future[Elem]
  def webJars(groupId: GroupId): Future[List[WebJar]]
  def webJarsSorted(maybeGroupdId: Option[GroupId] = None, dateTime: LocalDateTime = LocalDateTime.now().minusMonths(1)): Future[List[WebJar]]
  def getStats(groupId: GroupId, dateTime: LocalDateTime): Future[Map[(String, String), Int]]

  def asc(toSign: Array[Byte]): Option[String]

  def upload(gav: GAV, jar: Array[Byte], pom: String): Option[(DeploymentId, () => Option[CheckStatusResponse])]
  def publish(deploymentId: DeploymentId): Option[Unit]

  val maxwait = 1.minute
  val poll = 5.seconds

  def waitForDeploymentState(deploymentState: DeploymentState, f: () => Option[CheckStatusResponse])(implicit futures: Futures, actorSystem: ActorSystem): Future[Unit] = {
    val future = f() match {
      case Some(checkStatusResponse) if checkStatusResponse.deploymentState == deploymentState => Future.successful(())
      case Some(checkStatusResponse) if checkStatusResponse.deploymentState == DeploymentState.FAILED => Future.failed(new IllegalStateException("Failed to deploy"))
      case _ => after(poll)(waitForDeploymentState(deploymentState, f))
    }

    future.withTimeout(maxwait)
  }

}

@Singleton
class MavenCentralLive @Inject() (memcache: Memcache, wsClient: WSClient, configuration: Configuration, webJarsFileService: WebJarsFileService, environment: Environment, futures: Futures)
                                 (allDeployables: AllDeployables)
                                 (implicit ec: ExecutionContext, actorSystem: ActorSystem) extends MavenCentral with Logging {
  import MavenCentral._

  private implicit val transcoderOptionalInt: Transcoder[Option[Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Option[Int]]]
  private implicit val transcoderNameUrl: Transcoder[(String, String)] = new SerializingTranscoder().asInstanceOf[Transcoder[(String, String)]]
  private implicit val transcoderStats: Transcoder[Map[(String, String), Int]] = new SerializingTranscoder().asInstanceOf[Transcoder[Map[(String, String), Int]]]

  private lazy val maybeLimit = configuration.getOptional[Int]("mavencentral.limit").orElse(Option.when(environment.mode == Mode.Dev)(5))

  private lazy val ossProject = configuration.get[String]("oss.project")
  private lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)

  val browser = JsoupBrowser()

  def withOssCredentials[T](f: (String, String) => Future[T]): Future[T] = {
    val maybeUsernameAndPassword = for {
      ossUsername <- maybeOssUsername(configuration)
      ossPassword <- maybeOssPassword(configuration)
    } yield (ossUsername, ossPassword)

    maybeUsernameAndPassword.fold(Future.failed[T](new NoAccess("oss.username or oss.password not set"))) { case (ossUsername, ossPassword) =>
      f(ossUsername, ossPassword)
    }
  }

  def withOssDeployCredentials[T](f: (String, String) => Option[T]): Option[T] = {
    for {
      deployUsername <- maybeOssDeployUsername(configuration)
      deployPassword <- maybeOssDeployPassword(configuration)
      result <- f(deployUsername, deployPassword)
    } yield result
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

  // this is trash but it is more reliable than search.maven.org
  //  because that site does severe rate limiting and has a low page size which results in lots of reqs needed to fetch the list of webjars
  //  and you'll get denied. and it won't feel good after you've spent hours writing this awfulness only to realize it won't work in production

  def fetchDirs(url: String): Future[Set[String]] = {
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

  def gavsToWebJarVersion(gavs: Set[GAV]): Future[Set[WebJarVersion]] = {
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

  def convertToFutureOption[A](opt: Option[Future[A]])(implicit ec: ExecutionContext): Future[Option[A]] = {
    opt match {
      case Some(future) => future.map(Some(_))
      case None => Future.successful(None)
    }
  }

  def fetchDetails(groupId: GroupId, artifactId: ArtifactId): Future[Option[WebJar]] = {
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

  import java.io.ByteArrayOutputStream
  import java.util.zip.{ZipEntry, ZipOutputStream}

  private def createZip(files: Map[String, Array[Byte]]): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val zos = new ZipOutputStream(baos)

    files.foreach { case (filename, content) =>
      zos.putNextEntry(new ZipEntry(filename))
      zos.write(content)
      zos.closeEntry()
    }

    zos.close()
    baos.toByteArray
  }

  private def withSonatypeClient[T](f: SyncSonatypeClient => Option[T]): Option[T] = {
    withOssDeployCredentials { (username, password) =>
      val credentials = SonatypeCredentials(username, password)
      val client = new SyncSonatypeClient(credentials)
      f(client)
    }
  }

  override def upload(gav: GAV, jar: Array[Byte], pom: String): Option[(DeploymentId, () => Option[CheckStatusResponse])] = {
    withSonatypeClient { client =>
      for {
        pomAsc <- asc(pom.getBytes)
        jarAsc <- asc(jar)
      } yield {
        val deploymentName = DeploymentName(gav.toString)

        val files = Map(
          s"${gav.path}.jar" -> jar,
          s"${gav.path}.jar.sha1" -> jar.sha1.hex.getBytes,
          s"${gav.path}.jar.md5" -> jar.md5.hex.getBytes,
          s"${gav.path}.jar.asc" -> jarAsc.getBytes,

          s"${gav.path}.pom" -> pom.getBytes,
          s"${gav.path}.pom.sha1" -> pom.sha1.hex.getBytes,
          s"${gav.path}.pom.md5" -> pom.md5.hex.getBytes,
          s"${gav.path}.pom.asc" -> pomAsc.getBytes,
        )

        val zip = createZip(files)

        val deploymentId = client
          .uploadBundleFromBytes(
            zip,
            deploymentName,
            Some(USER_MANAGED)
          )

        deploymentId -> { () => client.checkStatus(deploymentId) }
      }
    }
  }

  override def publish(deploymentId: DeploymentId): Option[Unit] = {
    if (disableDeploy) {
      None
    }
    else {
      withSonatypeClient { client =>
        client.publishValidatedDeployment(deploymentId)
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
}
