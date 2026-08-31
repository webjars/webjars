package webjars.utils

import zio.*
import zio.compress.GzipDecompressor
import zio.stream.*

object Gzip:

  private val Magic = Chunk(0x1f.toByte, 0x8b.toByte)

  val decompress: ZPipeline[Any, Throwable, Byte, Byte] =
    GzipDecompressor.decompress

  val decompressOrIdentity: ZPipeline[Any, Throwable, Byte, Byte] =
    ZPipeline.branchAfter[Any, Throwable, Byte, Byte](Magic.length): prefix =>
      if prefix == Magic then ZPipeline.prepend(prefix) >>> decompress
      else ZPipeline.prepend(prefix)
