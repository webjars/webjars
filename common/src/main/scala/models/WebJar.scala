package models

import utils.VersionStringOrdering

case class WebJar(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion])

case class WebJarVersion(number: String, numFiles: Option[Int] = None)



object WebJarVersion {
  // todo, this doesn't work on date-based versions that follow non-standard formats (e.g. ace)
  given Ordering[WebJarVersion] with
    override def compare(a: WebJarVersion, b: WebJarVersion): Int = VersionStringOrdering.compare(a.number, b.number)
}
