package utils

import play.api.libs.ws.WS
import play.api.libs.json.{JsValue, Json, JsObject}
import play.api.cache.Cache
import play.api.Play.current
import play.api.{Logger, Play}

import models.{WebJarVersion, WebJar}

import sun.net.www.protocol.jar.JarURLConnection
import java.net.{URLEncoder, URL}
import java.util.jar.JarEntry

import scala.concurrent.{Promise, Future}
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.concurrent.ExecutionContext.Implicits.global

object MavenCentral {

  implicit val webJarVersionReads = Json.reads[WebJarVersion]
  implicit val webJarVersionWrites = Json.writes[WebJarVersion]
  
  implicit val webJarReads = Json.reads[WebJar]
  implicit val webJarWrites = Json.writes[WebJar]

  val ALL_WEBJARS_CACHE_KEY: String = "allWebJars"
  
  def allWebJars: Future[Iterable[WebJar]] = {
    Cache.getAs[JsValue](ALL_WEBJARS_CACHE_KEY).map { webjarsJson =>
      Promise().success(webjarsJson.as[Iterable[WebJar]]).future
    } getOrElse {
      
      // todo: would be nice if this could only happen only once no matter how many in-flight requests have missed the cache
      WS.url(Play.configuration.getString("webjars.searchGroupUrl").get).get().map { response =>
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
          webJarVersions.sortBy(_.number).reverse
        }
        
        val webjarsUnsorted = grouped.filterNot { webjar =>
          webjar._1.startsWith("webjars-") // remove items like "webjars-play"
        }.map { webjar =>
          WebJar(webjar._1, webjar._1, "http://github.com/webjars/" + webjar._1, webjar._2) // todo: find a way to get the actual name
        }

        val webjars = webjarsUnsorted.toArray.sortWith(_.name.toLowerCase < _.name.toLowerCase)
        
        Cache.set(ALL_WEBJARS_CACHE_KEY, Json.toJson(webjars), 60 * 60)

        webjars
      }
    }
  }
  
  def listFiles(artifactId: String, version: String): String = {
    val files = Cache.getOrElse[String](WebJarVersion.cacheKey(artifactId, version)) {
      
      val url = new URL(Play.configuration.getString("webjars.jarUrl").get.format(artifactId, URLEncoder.encode(version), artifactId, URLEncoder.encode(version)))

      val jarFileEntries: Iterator[JarEntry] = url.openConnection().asInstanceOf[JarURLConnection].getJarFile.entries()
      
      val webjarFiles: List[String] = jarFileEntries.filterNot { jarFileEntry =>
        jarFileEntry.isDirectory
      }.map { jarFileEntry =>
        jarFileEntry.getName
      }.filter { jarFile =>
        jarFile.startsWith("META-INF/resources/webjars")
      }.toList
      
      val webjarFilesJson = Json.toJson(webjarFiles)

      Cache.set(WebJarVersion.cacheKey(artifactId, version), webjarFilesJson.toString)

      // update the webjars cache to contain the new value
      /*
      Cache.getAs[Iterable[WebJar]](ALL_WEBJARS_CACHE_KEY).map { allWebJars =>
        allWebJars.map { webjar =>
        }
      }
      */
      // Brute force eviction
      Cache.set(ALL_WEBJARS_CACHE_KEY, None)
      
      webjarFilesJson.toString
    }
    Json.parse(files).as[List[String]].mkString("\n")
  }

}
