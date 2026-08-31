package webjars.utils

import webjars.utils.Deployable.ArchiveStream
import zio.*
import zio.compress.{ArchiveEntry, ZipArchiver}
import zio.stream.*

object ArchiveCreator:

  final case class ArchiveFile(
    name: String,
    content: ArchiveStream,
    uncompressedSize: Option[Long] = None,
  )

  /** Stream a ZIP from an outer stream of entries and their content streams. */
  def archive(files: ZStream[Any, Throwable, ArchiveFile]): ArchiveStream =
    files
      .map: file =>
        val entry: ArchiveEntry[Option, Any] = ArchiveEntry(
          name = file.name,
          uncompressedSize = file.uncompressedSize,
        )
        entry -> file.content
      .via(ZipArchiver.archive)

  /** Convenience for files already materialized by an upstream API. */
  def archiveFiles(files: Map[String, Chunk[Byte]]): ArchiveStream =
    archive:
      ZStream.fromIterable(files.toSeq.sortBy(_._1)).map: (path, bytes) =>
        ArchiveFile(
          name = path,
          content = ZStream.fromChunk(bytes),
          uncompressedSize = Some(bytes.length.toLong),
        )
