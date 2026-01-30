package models

import play.api.libs.json.{Format, Json}
import utils.VersionStringOrdering

case class WebJar(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion]) extends Serializable

case class WebJarVersion(number: String, numFiles: Option[Int] = None)

object WebJar {
  given Format[WebJarVersion] = Json.format[WebJarVersion]
  given Format[WebJar] = Json.format[WebJar]
}

object WebJarVersion {
  // todo, this doesn't work on date-based versions that follow non-standard formats (e.g. ace)
  given Ordering[WebJarVersion] with
    override def compare(a: WebJarVersion, b: WebJarVersion): Int = VersionStringOrdering.compare(b.number, a.number) // reverse order
}
