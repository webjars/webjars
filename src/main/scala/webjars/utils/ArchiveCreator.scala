package webjars.utils

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}
import zio.*

import java.io.*
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

object ArchiveCreator:

  def tarDir(dir: File, excludes: Set[File] = Set.empty[File]): ZIO[Any, Throwable, InputStream] =
    ZIO.attemptBlocking {
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
        val relativePath = dir.toPath.relativize(file.toPath).toString
        val entry = new TarArchiveEntry(relativePath)
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
