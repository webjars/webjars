package webjars.models

import webjars.utils.VersionStringOrdering
import zio.json.*

case class WebJar(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion])

case class WebJarVersion(number: String, numFiles: Option[Int] = None)

object WebJar:
  given JsonEncoder[WebJarVersion] = DeriveJsonEncoder.gen[WebJarVersion]
  given JsonDecoder[WebJarVersion] = DeriveJsonDecoder.gen[WebJarVersion]
  given JsonEncoder[WebJar] = DeriveJsonEncoder.gen[WebJar]
  given JsonDecoder[WebJar] = DeriveJsonDecoder.gen[WebJar]

object WebJarVersion:
  given Ordering[WebJarVersion] with
    override def compare(a: WebJarVersion, b: WebJarVersion): Int = VersionStringOrdering.compare(b.number, a.number) // reverse order
