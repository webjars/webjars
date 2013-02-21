package models

import play.api.libs.json.{Reads, JsValue}

case class WebJar(artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion])

case class WebJarVersion(number: String, numFiles: Option[Int])

object WebJarVersionCompanion {
  
  def cacheKey(artifactId: String, version: String): String = artifactId + "-" + version + "-files"
  
}