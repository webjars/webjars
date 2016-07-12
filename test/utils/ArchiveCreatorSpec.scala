package utils

import java.io.{BufferedInputStream, File}
import java.nio.file.Files

import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.specs2.mutable.Specification


class ArchiveCreatorSpec extends Specification {

  val tmpDir = Files.createTempDirectory("archive-utils-spec").toFile

  "tarDir" should {
    "tar up a dir with nested dirs" in {
      val testDir = new File(tmpDir, "1")
      testDir.mkdir()

      val a = new File(testDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)

      val b = new File(testDir, "b")
      b.mkdir()

      val c = new File(b, "c.txt")
      Files.write(c.toPath, "test".getBytes)

      val tarTry = ArchiveCreator.tarDir(testDir)
      tarTry must beASuccessfulTry

      val bufferedInputStream = new BufferedInputStream(tarTry.get)

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)
      archiveStream.getNextEntry.getName must beEqualTo ("a.txt")
      archiveStream.getNextEntry.getName must beEqualTo ("b/c.txt")
      archiveStream.getNextEntry must beNull
    }
    "tar with file excludes" in {
      val testDir = new File(tmpDir, "2")
      testDir.mkdir()

      val a = new File(testDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)

      val b = new File(testDir, "b.txt")
      Files.write(b.toPath, "test".getBytes)

      val tarTry = ArchiveCreator.tarDir(testDir, Set(new File(testDir, "a.txt")))
      tarTry must beASuccessfulTry

      val bufferedInputStream = new BufferedInputStream(tarTry.get)

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)
      archiveStream.getNextEntry.getName must beEqualTo ("b.txt")
      archiveStream.getNextEntry must beNull
    }
    "tar with dir excludes" in {
      val testDir = new File(tmpDir, "3")
      testDir.mkdir()

      val a = new File(testDir, "a.txt")
      Files.write(a.toPath, "test".getBytes)

      val b = new File(testDir, "b")
      b.mkdir()

      val c = new File(b, "c.txt")
      Files.write(c.toPath, "test".getBytes)

      val tarTry = ArchiveCreator.tarDir(testDir, Set(new File(testDir, "b")))
      tarTry must beASuccessfulTry

      val bufferedInputStream = new BufferedInputStream(tarTry.get)

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)
      archiveStream.getNextEntry.getName must beEqualTo ("a.txt")
      archiveStream.getNextEntry must beNull
    }
  }

  step(tmpDir.delete())

}
