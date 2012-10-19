package utils

import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS
import play.api.libs.json.JsObject
import play.api.cache.Cache
import play.api.Play.current
import play.api.Play

import models.WebJar

object MavenCentral {

  def allWebJars: Promise[Iterable[WebJar]] = {
    Cache.getAs[Iterable[WebJar]]("allWebJars").map {
      Promise.pure(_)
    } getOrElse {
      // todo: would be nice if this could only happen only once no matter how many in-flight requests have missed the cache
      WS.url(Play.configuration.getString("webjars.searchGroupUrl").get).get().map { response =>
        val allVersions = (response.json \ "response" \ "docs").as[Seq[JsObject]].map { jsObject =>
          ((jsObject \ "a").as[String], (jsObject \ "v").as[String])
        }
        
        val grouped = allVersions.groupBy(_._1)  // group by the artifactId
        
        val webjars = grouped.map { version =>
          val versions = version._2.map(_._2) // create a list of the versions
          WebJar(version._1, version._1, "http://github.com/webjars/" + version._1, versions) // todo: find a way to get the actual name
        }
        
        Cache.set("allWebJars", webjars, 60 * 60)
        webjars
      }
    }
  }

}
