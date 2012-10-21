package utils

import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS
import play.api.libs.json.JsObject
import play.api.cache.Cache
import play.api.Play.current
import play.api.Play

import models.{WebJarVersion, WebJar}

import sun.net.www.protocol.jar.JarURLConnection
import java.net.URL
import scala.collection.JavaConversions.enumerationAsScalaIterator
import java.util.jar.JarEntry

object MavenCentral {

  val ALL_WEBJARS_CACHE_KEY: String = "allWebJars"
  
  def allWebJars: Promise[Iterable[WebJar]] = {
    Cache.getAs[Iterable[WebJar]](ALL_WEBJARS_CACHE_KEY).map {
      Promise.pure(_)
    } getOrElse {
      // todo: would be nice if this could only happen only once no matter how many in-flight requests have missed the cache
      WS.url(Play.configuration.getString("webjars.searchGroupUrl").get).get().map { response =>
        val allVersions = (response.json \ "response" \ "docs").as[Seq[JsObject]].map { jsObject =>
          ((jsObject \ "a").as[String], (jsObject \ "v").as[String])
        }

        // group by the artifactId and pull the list of files from the cache
        val grouped = allVersions.groupBy(_._1).mapValues {
          _.map { webJarVersion =>
            WebJarVersion(webJarVersion._2, Cache.getAs[Seq[String]](WebJarVersion.cacheKey(webJarVersion._1, webJarVersion._2)))
          }
        }
        
        val webjars = grouped.map { webjar =>
          WebJar(webjar._1, webjar._1, "http://github.com/webjars/" + webjar._1, webjar._2) // todo: find a way to get the actual name
        }
        
        Cache.set(ALL_WEBJARS_CACHE_KEY, webjars, 60 * 60)
        webjars
      }
    }
  }
  
  def listFiles(artifactId: String, version: String): String = {
    val files = Cache.getOrElse[Seq[String]](WebJarVersion.cacheKey(artifactId, version)) {

      val url = new URL(Play.configuration.getString("webjars.jarUrl").get.format(artifactId, version, artifactId, version))

      val jarFileEntries: Iterator[JarEntry] = url.openConnection().asInstanceOf[JarURLConnection].getJarFile.entries()
      
      val webjarFiles: Seq[String] = jarFileEntries.filterNot { jarFileEntry =>
        jarFileEntry.isDirectory
      }.map { jarFileEntry =>
        jarFileEntry.getName
      }.filter { jarFile =>
        jarFile.startsWith("META-INF/resources/webjars")
      }.toSeq

      Cache.set(WebJarVersion.cacheKey(artifactId, version), webjarFiles)

      // update the webjars cache to contain the new value
      /*
      Cache.getAs[Iterable[WebJar]](ALL_WEBJARS_CACHE_KEY).map { allWebJars =>
        allWebJars.map { webjar =>
        }
      }
      */
      // Brute force eviction
      Cache.set(ALL_WEBJARS_CACHE_KEY, None)
      
      webjarFiles
    }
    files.mkString("\n")
  }

}
