package webjars

import webjars.utils.{ArchiveCreator, WebJarCreator}
import webjars.utils.Deployable.ArchiveStream
import zio.*
import zio.compress.*
import zio.stream.*
import zio.test.*

object ArchiveCreatorSpec extends ZIOSpecDefault:

  private def entries(archive: ArchiveStream): Task[Chunk[(String, Chunk[Byte])]] =
    WebJarCreator.unarchiveStream(archive)
      .map { case (name, _, content) => name -> content }
      .runCollect

  def spec = suite("ArchiveCreator")(
    test("archives in-memory files") {
      val files = Map(
        "package.json" -> Chunk.fromArray("{}".getBytes),
        "dist/app.js"  -> Chunk.fromArray("app".getBytes),
      )
      entries(ArchiveCreator.archiveFiles(files)).map { archived =>
        assertTrue(
          archived.map(_._1) == Chunk("dist/app.js", "package.json"),
          archived.toMap == files,
        )
      }
    },
    test("supports paths longer than the tar header limit") {
      val path = "this/is/a/really/long/path/which/is/longer/than/tar/normally/supports/so-we-need-to-make-sure-it-works/a.txt"
      val files = Map(path -> Chunk.fromArray("test".getBytes))
      entries(ArchiveCreator.archiveFiles(files)).map { archived =>
        assertTrue(
          path.getBytes.length > 100,
          archived.map(_._1) == Chunk(path),
          archived.toMap == files,
        )
      }
    },
    test("streaming archive detection handles tar input") {
      val bytes = Chunk.fromArray("a".getBytes)
      val entry: ArchiveEntry[Some, Any] = ArchiveEntry(
        name = "a.txt",
        uncompressedSize = Some(bytes.length.toLong),
      )
      val tar = ZStream(entry -> (ZStream.fromChunk(bytes): ArchiveStream))
        .via(TarArchiver.archive)
      entries(tar).map { archived =>
        assertTrue(archived == Chunk("a.txt" -> bytes))
      }
    },
  )
