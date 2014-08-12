package utils

import play.api.libs.ws.WS
import play.api.libs.json.{JsValue, Json, JsObject}
import play.api.cache.Cache
import play.api.Play.current
import play.api.{Logger, Play}

import models.{WebJarVersion, WebJar}

import sun.net.www.protocol.jar.JarURLConnection
import java.net.{URLEncoder, URL}
import java.util.jar.{JarFile, JarEntry}

import scala.concurrent.{Promise, Future}
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.IOException

import scala.xml.Elem

object MavenCentral {

  implicit val webJarVersionReads = Json.reads[WebJarVersion]
  implicit val webJarVersionWrites = Json.writes[WebJarVersion]
  
  implicit val webJarReads = Json.reads[WebJar]
  implicit val webJarWrites = Json.writes[WebJar]

  val ALL_WEBJARS_CACHE_KEY: String = "allWebJars"
  
  def allWebJars: Future[Iterable[WebJar]] = {
    Cache.getAs[JsValue](ALL_WEBJARS_CACHE_KEY).map { webjarsJson =>
      Future.successful(webjarsJson.as[Iterable[WebJar]])
    } getOrElse {
      // todo: would be nice if this could only happen only once no matter how many in-flight requests have missed the cache
      WS.url(Play.configuration.getString("webjars.searchGroupUrl").get).get().flatMap { response =>

        val allVersions = (response.json \ "response" \ "docs").as[Seq[JsObject]].map { jsObject =>
          ((jsObject \ "a").as[String], (jsObject \ "v").as[String])
        }

        // group by the artifactId and pull the list of files from the cache
        val grouped = allVersions.groupBy(_._1).mapValues { versions =>
          val webJarVersions = versions.map { webJarVersion =>
            WebJarVersion(webJarVersion._2, Cache.getAs[String](WebJarVersion.cacheKey(webJarVersion._1, webJarVersion._2)).map { files =>
              Json.parse(files).as[List[String]].length
            })
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
          val webjars = webjarsUnsorted.toArray.sortWith(_.name.toLowerCase < _.name.toLowerCase)

          Cache.set(ALL_WEBJARS_CACHE_KEY, Json.toJson(webjars), 60 * 60)

          webjars
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
  
  def listFiles(baseUrl: String, artifactId: String, version: String): String = {
    val files = Cache.getOrElse[String](WebJarVersion.cacheKey(artifactId, version)) {

      val maybeJarFile = getFile(baseUrl, artifactId, version)

      val jarFileEntries: Iterator[JarEntry] = maybeJarFile.map(_.entries().toIterator).getOrElse(Iterator.empty)
      
      val webjarFiles: List[String] = jarFileEntries.filterNot { jarFileEntry =>
        jarFileEntry.isDirectory
      }.map { jarFileEntry =>
        jarFileEntry.getName
      }.filter { jarFile =>
        jarFile.startsWith("META-INF/resources/webjars")
      }.toList

      maybeJarFile.foreach(_.close())
      
      val webjarFilesJson = Json.toJson(webjarFiles)

      Cache.set(WebJarVersion.cacheKey(artifactId, version), webjarFilesJson.toString())

      // update the webjars cache to contain the new value
      /*
      Cache.getAs[Iterable[WebJar]](ALL_WEBJARS_CACHE_KEY).map { allWebJars =>
        allWebJars.map { webjar =>
        }
      }
      */
      // Brute force eviction
      Cache.set(ALL_WEBJARS_CACHE_KEY, None)
      
      webjarFilesJson.toString()
    }
    Json.parse(files).as[List[String]].mkString("\n")
  }

  def getFile(baseUrl: String, artifactId: String, version: String): Option[JarFile] = {
    try {
      val url = new URL(baseUrl.format(artifactId, URLEncoder.encode(version, "UTF-8"), artifactId, URLEncoder.encode(version, "UTF-8")))
      Some(url.openConnection().asInstanceOf[JarURLConnection].getJarFile)
    } catch {
      case e: Exception => None
    }
  }

}
