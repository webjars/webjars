package models

import play.api.libs.json.{Format, Json}
import utils.VersionStringOrdering

case class WebJar(`type`: String, groupId: String, artifactId: String, name: String, sourceUrl: String, versions: List[WebJarVersion]) extends Serializable

case class WebJarVersion(number: String, numFiles: Int = 0)

object WebJar {
  implicit val webJarVersionFormat: Format[WebJarVersion] = Json.format[WebJarVersion]
  implicit val webJarFormat: Format[WebJar] = Json.format[WebJar]
}

object WebJarVersion {

  // todo, this doesn't work on date-based versions that follow non-standard formats (e.g. ace)
  implicit object WebJarVersionOrdering extends Ordering[WebJarVersion] {
    override def compare(a: WebJarVersion, b: WebJarVersion): Int = VersionStringOrdering.compare(a.number, b.number)
  }

}
