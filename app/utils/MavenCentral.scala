package utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, InputStream}
import java.net.URLEncoder
import java.nio.file.Files
import java.util.jar.JarInputStream
import java.util.zip.{DeflaterOutputStream, InflaterInputStream}

import actors.{FetchWebJars, WebJarFetcher}
import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.ning.http.client.providers.netty.NettyResponse
import models.WebJarCatalog
import models.WebJarCatalog.WebJarCatalog
import models.{WebJarCatalog, WebJar, WebJarVersion}
import org.webjars.WebJarAssetLocator
import play.api.Play.current
import play.api.cache.Cache
import play.api.http.Status
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.{WS, WSResponse}
import play.api.{Logger, Play}
import shade.memcached.Codec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.Source
import scala.xml.{Elem, XML}

object MavenCentral {

  lazy val webJarFetcher: ActorRef = Akka.system.actorOf(Props[WebJarFetcher])

  lazy val tempDir: File = {
    Files.createTempDirectory("webjars").toFile
  }

  implicit val webJarVersionReads = Json.reads[WebJarVersion]
  implicit val webJarVersionWrites = Json.writes[WebJarVersion]

  implicit val webJarReads = Json.reads[WebJar]
  implicit val webJarWrites = Json.writes[WebJar]

  // from: http://stackoverflow.com/questions/15079332/round-tripping-through-deflater-in-scala-fails

  def compress(bytes: Array[Byte]): Array[Byte] = {
    val deflater = new java.util.zip.Deflater
    val baos = new ByteArrayOutputStream
    val dos = new DeflaterOutputStream(baos, deflater)
    dos.write(bytes)
    dos.finish()
    dos.close()
    baos.close()
    deflater.end()
    baos.toByteArray
  }

  def decompress(bytes: Array[Byte]): Array[Byte] = {
    val inflater = new java.util.zip.Inflater()
    val bytesIn = new ByteArrayInputStream(bytes)
    val in = new InflaterInputStream(bytesIn, inflater)
    val out = Source.fromInputStream(in).map(_.toByte).toArray
    in.close()
    bytesIn.close()
    inflater.end()
    out
  }

  implicit object StringsCodec extends Codec[List[String]] {
    def serialize(fileList: List[String]): Array[Byte] = compress(Json.toJson(fileList).toString().getBytes)
    def deserialize(data: Array[Byte]): List[String] = Json.parse(decompress(data)).as[List[String]]
  }

  implicit object ElemCode extends Codec[Elem] {
    def serialize(elem: Elem): Array[Byte] = compress(elem.toString().getBytes)
    def deserialize(data: Array[Byte]): Elem = XML.loadString(new String(decompress(data)))
  }

  val primaryBaseJarUrl = Play.current.configuration.getString("webjars.jarUrl.primary").get
  val fallbackBaseJarUrl = Play.current.configuration.getString("webjars.jarUrl.fallback").get

  def fetchWebJarNameAndUrl(artifactId: String, version: String): Future[(String, String)] = {
    getPom(artifactId, version).flatMap { xml =>
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
          getPom(parentArtifactId, version).map { parentXml =>
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
              MavenCentral.getFileList(artifactId, version).map { fileList =>
                WebJarVersion(version, fileList.length)
              }
            }
          } map { webJarVersions =>
            webJarVersions.sorted.reverse
          }
          artifactId -> webJarVersionsFuture
      }

      val webJarsFuture: Future[List[WebJar]] = Future.traverse(webJarsWithFutureVersions) {
        case (artifactId, webJarVersionsFuture) =>
          webJarVersionsFuture.flatMap { webJarVersions =>
            MavenCentral.fetchWebJarNameAndUrl(artifactId, webJarVersions.map(_.number).head).map {
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
      Akka.system.actorSelection(catalog.toString).resolveOne(1.second).flatMap { actorRef =>
        // in-flight request exists
        Future.failed(new Exception("Existing request for WebJars"))
      } recoverWith {
        // no request so make one
        case _ =>
          implicit val timeout = Timeout(10.minutes)
          val webJarFetcher = Akka.system.actorOf(Props(classOf[WebJarFetcher], catalog), catalog.toString)
          val fetchWebJarsFuture = (webJarFetcher ? FetchWebJars).mapTo[List[WebJar]]
          fetchWebJarsFuture.foreach { fetchedWebJars =>
            Akka.system.stop(webJarFetcher)
            Cache.set(catalog.toString, fetchedWebJars, 1.hour)
          }
          // fail cause this is will likely take a long time
          Future.failed(new Exception("Making new request for WebJars"))
      }
    }
  }

  private def fetchPom(id: String, version: String): Future[Elem] = {
    val url = s"http://repo1.maven.org/maven2/org/webjars/$id/$version/$id-$version.pom"
    WS.url(url).get().map(_.xml)
  }

  def getPom(id: String, version: String): Future[Elem] = {
    val cacheKey = s"pom-$id"
    Global.memcached.get[Elem](cacheKey).flatMap { maybeElem =>
      maybeElem.map(Future.successful).getOrElse {
        val pomFuture = fetchPom(id, version)
        pomFuture.foreach { pom =>
          Global.memcached.set(cacheKey, pom, Duration.Inf)
        }
        pomFuture
      }
    }
  }

  private def fetchFileList(artifactId: String, version: String): Future[List[String]] = {
    getFile(artifactId, version).map { jarInputStream =>
      val webJarFiles = Stream.continually(jarInputStream.getNextJarEntry).
        takeWhile(_ != null).
        filterNot(_.isDirectory).
        map(_.getName).
        filter(_.startsWith(WebJarAssetLocator.WEBJARS_PATH_PREFIX)).
        toList
      jarInputStream.close()
      webJarFiles
    }
  }

  def getFileList(artifactId: String, version: String): Future[List[String]] = {
    val cacheKey = WebJarVersion.cacheKey(artifactId, version)
    Global.memcached.get[List[String]](cacheKey).flatMap { maybeFileList =>
      maybeFileList.map(Future.successful).getOrElse {
        val fileListFuture = fetchFileList(artifactId, version)
        fileListFuture.foreach { fileList =>
          Global.memcached.set(cacheKey, fileList, Duration.Inf)
        }
        fileListFuture
      }
    }
  }

  def getFile(artifactId: String, version: String): Future[JarInputStream] = {
    val tmpFile = new File(tempDir, s"$artifactId-$version.jar")

    if (tmpFile.exists()) {
      Future.successful(new JarInputStream(Files.newInputStream(tmpFile.toPath)))
    }
    else {
      val fileInputStreamFuture = getFileInputStream(primaryBaseJarUrl, artifactId, version).recoverWith {
        case _ =>
          getFileInputStream(fallbackBaseJarUrl, artifactId, version)
      }

      fileInputStreamFuture.map { fileInputStream =>
        // todo: not thread safe!
        // write to the fs
        Files.copy(fileInputStream, tmpFile.toPath)
        // read it from the fs since we've drained the http response
        new JarInputStream(Files.newInputStream(tmpFile.toPath))
      }
    }
  }

  def getFileInputStream(baseJarUrl: String, artifactId: String, version: String): Future[InputStream] = {
    val url = baseJarUrl.format(artifactId, URLEncoder.encode(version, "UTF-8"), artifactId, URLEncoder.encode(version, "UTF-8"))
    WS.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful(response.underlying[NettyResponse].getResponseBodyAsStream)
        case Status.NOT_FOUND =>
          Future.failed(NotFoundResponseException(response))
        case _ =>
          Logger.error(s"Unexpected response retrieving $url - ${response.statusText} ${response.body}")
          Future.failed(UnexpectedResponseException(response))
      }
    }
  }

  case class NotFoundResponseException(response: WSResponse) extends RuntimeException {
    override def getMessage: String = response.statusText
  }

  case class UnexpectedResponseException(response: WSResponse) extends RuntimeException {
    override def getMessage: String = response.statusText
  }

}
