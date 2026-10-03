package webjars.models

import webjars.utils.VersionStringOrdering
import zio.NonEmptyChunk
import zio.json.*

// `versions` is non-empty by construction: every view renders the first
// version (dependency snippet, file-list link), so a WebJar without versions
// is an illegal state. Cache entries are parsed into this type via
// `WebJar.fromCache`, which rejects version-less entries at the boundary.
case class WebJar(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: NonEmptyChunk[WebJarVersion]):
  def latestVersion: WebJarVersion = versions.head

case class WebJarVersion(number: String, numFiles: Option[Int] = None)

object WebJar:
  given JsonEncoder[WebJarVersion] = DeriveJsonEncoder.gen[WebJarVersion]
  given JsonDecoder[WebJarVersion] = DeriveJsonDecoder.gen[WebJarVersion]
  given JsonEncoder[WebJar] = DeriveJsonEncoder.gen[WebJar]
  given JsonDecoder[WebJar] = DeriveJsonDecoder.gen[WebJar]

  def fromCache(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: Iterable[WebJarVersion]): Option[WebJar] =
    NonEmptyChunk.fromIterableOption(versions).map(WebJar(groupId, artifactId, name, sourceUrl, _))

object WebJarVersion:
  given Ordering[WebJarVersion] with
    override def compare(a: WebJarVersion, b: WebJarVersion): Int = VersionStringOrdering.compare(b.number, a.number) // reverse order
