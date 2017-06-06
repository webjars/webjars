package models

import play.api.libs.json.Json
import utils.VersionOrdering

case class WebJar(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: List[WebJarVersion]) extends Serializable

case class WebJarVersion(number: String, numFiles: Int = 0)

object WebJar {
  implicit val webJarVersionFormat = Json.format[WebJarVersion]
  implicit val webJarFormat = Json.format[WebJar]
}

object WebJarVersion {

  // todo, this doesn't work on date-based versions that follow non-standard formats (e.g. ace)
  implicit object WebJarVersionOrdering extends Ordering[WebJarVersion] {
    override def compare(a: WebJarVersion, b: WebJarVersion): Int = VersionOrdering.compare(a.number, b.number)
  }

}
