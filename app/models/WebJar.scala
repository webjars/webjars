package models

import play.api.libs.json.Json

case class WebJar(artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion])

case class WebJarVersion(number: String, numFiles: Option[Int])

object WebJar {
  implicit val webJarVersionFormat = Json.format[WebJarVersion]
  implicit val webJarFormat = Json.format[WebJar]
}

object WebJarVersion {
  def cacheKey(artifactId: String, version: String): String = artifactId + "-" + version + "-files"
}