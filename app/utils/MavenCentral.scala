package utils

import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS
import play.api.libs.json.JsObject
import play.api.cache.Cache
import play.api.Play.current
import play.api.Play

import models.WebJar

object MavenCentral {

  def allWebJars: Promise[Seq[WebJar]] = {
    Cache.getAs[Seq[WebJar]]("allWebJars").map {
      Promise.pure(_)
    } getOrElse {
      // todo: would be nice if this could only happen only once no matter how many in-flight requests have missed the cache
      WS.url(Play.configuration.getString("webjars.searchGroupUrl").get).get().map { response =>
        val webjars = (response.json \ "response" \ "docs").as[Seq[JsObject]].map { jsObject =>
          (jsObject \ "versionCount").as[Int] match {
            case 1 => {
              val artifactId = (jsObject \ "a").as[String]
              WebJar(artifactId, artifactId, "http://github.com/webjars/" + artifactId, Seq((jsObject \ "latestVersion").as[String])) // todo: find a way to get the actual name
            }
            case _ => {
              // todo: get the list of all versions
              val artifactId = (jsObject \ "a").as[String]
              WebJar(artifactId, artifactId, "http://github.com/webjars/" + artifactId, Seq((jsObject \ "latestVersion").as[String])) // todo: find a way to get the actual name
            }
          }
        }
        println(webjars)
        Cache.set("allWebJars", webjars, 60 * 60)
        webjars
      }
    }
  }

}
