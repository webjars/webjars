package utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{InflaterInputStream, DeflaterOutputStream}

import com.ning.http.client.providers.netty.NettyResponse
import org.webjars.WebJarAssetLocator
import play.api.cache.Cache
import play.api.http.Status
import play.api.libs.ws.{WSResponse, WS}
import play.api.libs.json.{Json, JsObject}
import play.api.Play.current
import play.api.{Logger, Play}

import models.{WebJarVersion, WebJar}
import shade.memcached.Codec

import java.net.URLEncoder
import java.util.jar.JarInputStream

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.xml.Elem

object MavenCentral {

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
    baos.toByteArray
  }

  def decompress(bytes: Array[Byte]): Array[Byte] = {
    val inflater = new java.util.zip.Inflater()
    val bytesIn = new ByteArrayInputStream(bytes)
    val in = new InflaterInputStream(bytesIn, inflater)
    val out = Stream.continually(in.read).takeWhile(_ != -1).map(_.toByte).toArray
    in.close()
    bytesIn.close()
    out
  }


  implicit object WebJarsCodec extends Codec[List[WebJar]] {
    def serialize(webJars: List[WebJar]): Array[Byte] = compress(Json.toJson(webJars).toString().getBytes)
    def deserialize(data: Array[Byte]): List[WebJar] = Json.parse(decompress(data)).as[List[WebJar]]
  }

  implicit object StringsCodec extends Codec[List[String]] {
    def serialize(fileList: List[String]): Array[Byte] = compress(Json.toJson(fileList).toString().getBytes)
    def deserialize(data: Array[Byte]): List[String] = Json.parse(decompress(data)).as[List[String]]
  }

  val primaryBaseJarUrl = Play.current.configuration.getString("webjars.jarUrl.primary").get
  val fallbackBaseJarUrl = Play.current.configuration.getString("webjars.jarUrl.fallback").get

  val ALL_WEBJARS_CACHE_KEY: String = "allWebJars"

  private def fetchWebJars(): Future[List[WebJar]] = {
    // todo: would be nice if this could only happen only once no matter how many in-flight requests have missed the cache

    Logger.info("Getting the full WebJar list")

    WS.url(Play.configuration.getString("webjars.searchGroupUrl").get).get().flatMap { response =>

      val allVersions = (response.json \ "response" \ "docs").as[List[JsObject]].map { jsObject =>
        ((jsObject \ "a").as[String], (jsObject \ "v").as[String])
      }

      // group by the artifactId
      val grouped = allVersions.groupBy(_._1).mapValues { versions =>
        val webJarVersions = versions.map {
          case (artifactId, version) =>
            WebJarVersion(version, None)
        }
        webJarVersions.sorted.reverse
      }

      val webjarsUnsortedFutures = grouped.filterNot { webjar =>
        webjar._1.startsWith("webjars-") // remove items like "webjars-play"
      }.map { case(id, versions) =>
        fetchPom(id, versions).flatMap { xml =>
          val artifactId = (xml \ "artifactId").text
          val rawName = (xml \ "name").text
          val name = if (rawName.contains("${")) {
            // can't handle pom properties so fallback to id
            id
          } else {
            rawName
          }
          val rawUrl = (xml \ "scm" \ "url").text
          val url = if (rawUrl.contains("${")) {
            // can't handle pom properties so fallback to a guess
            s"http://github.com/webjars/$id"
          } else {
            rawUrl
          }
          if (url != "") {
            Future.successful(WebJar(artifactId, name, url, versions))
          }
          else {
            // try the parent pom
            val parentArtifactId = (xml \ "parent" \ "artifactId").text
            fetchPom(parentArtifactId, versions).map { parentXml =>
              val parentUrl = (parentXml \ "scm" \ "url").text
              WebJar(artifactId, name, parentUrl, versions)
            }
          }
        } recover {
          case _ =>
            WebJar(id, id, s"http://github.com/webjars/$id", versions)
        }
      }

      Future.sequence(webjarsUnsortedFutures).map { webjarsUnsorted =>
        webjarsUnsorted.toList.sortWith(_.name.toLowerCase < _.name.toLowerCase)
      }

    }
  }

  def allWebJars: Future[List[WebJar]] = {
    Global.memcached.get[List[WebJar]](ALL_WEBJARS_CACHE_KEY).flatMap { maybeWebJars =>
      val webJarsFuture = maybeWebJars.fold {
        val fetchWebJarsFuture = fetchWebJars()
        fetchWebJarsFuture.foreach { fetchedWebJars =>
          Global.memcached.set(ALL_WEBJARS_CACHE_KEY, fetchedWebJars, 1.hour)
        }
        fetchWebJarsFuture
      } (Future.successful)

      webJarsFuture.map { webJars =>
        // this list of WebJars doesn't have the number of files in each webjar version so try to get it from cache
        webJars.map { webJar =>
          val updatedVersions = webJar.versions.map { webJarVersion =>
            val cacheKey = WebJarVersion.cacheKey(webJar.artifactId, webJarVersion.number)
            val maybeNumFiles = Cache.getAs[Int](cacheKey)
            maybeNumFiles match {
              case Some(numFiles) =>
                webJarVersion.copy(numFiles = maybeNumFiles)
              case None =>
                // the local cache didn't have the number of files so get and set it
                listFiles(webJar.artifactId, webJarVersion.number).foreach { fileList =>
                  Cache.set(cacheKey, fileList.length, 24.hours)
                }
                webJarVersion
            }
          }
          webJar.copy(versions = updatedVersions)
        }
      }
    }
  }

  def fetchPom(id: String, versions: Seq[WebJarVersion]): Future[Elem] = {
    // todo: sort these first so we get the latest metadata
    val aVersion = versions.head.number
    val url = s"http://repo1.maven.org/maven2/org/webjars/$id/$aVersion/$id-$aVersion.pom"
    WS.url(url).get().map(_.xml)
  }

  private def getFileList(artifactId: String, version: String): Future[List[String]] = {
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

  def listFiles(artifactId: String, version: String): Future[List[String]] = {
    val cacheKey = WebJarVersion.cacheKey(artifactId, version)
    Global.memcached.get[List[String]](cacheKey).flatMap { maybeFileList =>
      maybeFileList.map(Future.successful).getOrElse {
        getFileList(artifactId, version).map { fileList =>
          Global.memcached.set(cacheKey, fileList, Duration.Inf)
          // update the in-memory cache of num files
          Cache.set(cacheKey, fileList.length, 24.hours)
          fileList
        }
      }
    }
  }

  def getFile(artifactId: String, version: String): Future[JarInputStream] = {
    getFile(primaryBaseJarUrl, artifactId, version).recoverWith {
      case _ =>
        getFile(fallbackBaseJarUrl, artifactId, version)
    }
  }

  def getFile(baseJarUrl: String, artifactId: String, version: String): Future[JarInputStream] = {
    val url = baseJarUrl.format(artifactId, URLEncoder.encode(version, "UTF-8"), artifactId, URLEncoder.encode(version, "UTF-8"))
    WS.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          Future.successful(new JarInputStream(new ByteArrayInputStream(response.underlying[NettyResponse].getResponseBodyAsBytes)))
        case _ =>
          Future.failed(new UnexpectedResponseException(response))
      }
    }
  }

  case class UnexpectedResponseException(response: WSResponse) extends RuntimeException {
    override def getMessage: String = response.statusText
  }

}
