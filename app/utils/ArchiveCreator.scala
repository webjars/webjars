package utils

import java.io._
import java.nio.file.Files

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}

import scala.jdk.CollectionConverters._
import scala.util.Try

object ArchiveCreator {

  def tarDir(dir: File, excludes: Set[File] = Set.empty[File]): Try[InputStream] = {
    Try {
      val files = Files
        .walk(dir.toPath).iterator().asScala
        .filter(_.toFile.isFile)
        .map(_.toFile)
        .filterNot(file => excludes.exists(exclude => file.getPath.startsWith(exclude.getPath)))

      val baos = new ByteArrayOutputStream()
      val bos = new BufferedOutputStream(baos)
      val tos = new TarArchiveOutputStream(bos)
      tos.setLongFileMode(TarArchiveOutputStream.LONGFILE_POSIX)

      files.foreach { file =>
        val entry = new TarArchiveEntry(file.getPath.stripPrefix(dir.getPath))
        entry.setSize(file.length())
        tos.putArchiveEntry(entry)
        Files.copy(file.toPath, tos)
        tos.closeArchiveEntry()
      }

      tos.close()
      bos.close()
      baos.close()

      new ByteArrayInputStream(baos.toByteArray)
    }
  }

}
