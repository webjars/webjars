package webjars

import webjars.utils.ArchiveCreator
import zio.*
import zio.compress.*
import zio.stream.*
import zio.test.*

import java.io.File
import java.nio.file.Files

object ArchiveCreatorSpec extends ZIOSpecDefault:

  def spec = suite("ArchiveCreator")(
    test("tar up a dir with nested dirs") {
      val tmpDir = Files.createTempDirectory("archive-spec-1").toFile
      val a = new File(tmpDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)
      val b = new File(tmpDir, "b")
      b.mkdir()
      val c = new File(b, "c.txt")
      Files.write(c.toPath, "test".getBytes)

      ArchiveCreator.tarDir(tmpDir).flatMap { tarStream =>
        ZStream.fromInputStream(tarStream)
          .via(TarUnarchiver.unarchive)
          .map(_._1.name)
          .runCollect
          .map { entryNames =>
            assertTrue(
              entryNames.size == 2,
              entryNames.contains("a.txt"),
              entryNames.contains("b/c.txt"),
            )
          }
      }.ensuring(ZIO.succeed(deleteRecursive(tmpDir)))
    },
    test("tar with file excludes") {
      val tmpDir = Files.createTempDirectory("archive-spec-2").toFile
      val a = new File(tmpDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)
      val b = new File(tmpDir, "b.txt")
      Files.write(b.toPath, "test".getBytes)

      ArchiveCreator.tarDir(tmpDir, Set(new File(tmpDir, "a.txt"))).flatMap { tarStream =>
        ZStream.fromInputStream(tarStream)
          .via(TarUnarchiver.unarchive)
          .map(_._1.name)
          .runCollect
          .map { entryNames =>
            assertTrue(
              entryNames.size == 1,
              entryNames.head == "b.txt",
            )
          }
      }.ensuring(ZIO.succeed(deleteRecursive(tmpDir)))
    },
    test("tar with dir excludes") {
      val tmpDir = Files.createTempDirectory("archive-spec-3").toFile
      val a = new File(tmpDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)
      val b = new File(tmpDir, "b")
      b.mkdir()
      val c = new File(b, "c.txt")
      Files.write(c.toPath, "test".getBytes)

      ArchiveCreator.tarDir(tmpDir, Set(new File(tmpDir, "b"))).flatMap { tarStream =>
        ZStream.fromInputStream(tarStream)
          .via(TarUnarchiver.unarchive)
          .map(_._1.name)
          .runCollect
          .map { entryNames =>
            assertTrue(
              entryNames.size == 1,
              entryNames.head == "a.txt",
            )
          }
      }.ensuring(ZIO.succeed(deleteRecursive(tmpDir)))
    },
    test("tar long dir names") {
      val tmpDir = Files.createTempDirectory("archive-spec-4").toFile
      val dirName = "this/is/a/really/long/dir/name/which/is/longer/than/zip/normally/supports/so-we-will-need-to-make-sure-it-works"
      val aDir = new File(tmpDir, dirName)
      aDir.mkdirs()
      val a = new File(aDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)

      ArchiveCreator.tarDir(tmpDir).flatMap { tarStream =>
        ZStream.fromInputStream(tarStream)
          .via(TarUnarchiver.unarchive)
          .map(_._1.name)
          .runCollect
          .map { entryNames =>
            assertTrue(
              dirName.getBytes.length > 100,
              entryNames.size == 1,
              entryNames.head == dirName + "/a.txt",
            )
          }
      }.ensuring(ZIO.succeed(deleteRecursive(tmpDir)))
    },
  )

  private def deleteRecursive(file: File): Unit =
    if file.isDirectory then file.listFiles().foreach(deleteRecursive)
    file.delete()
