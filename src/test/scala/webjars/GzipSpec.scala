package webjars

import webjars.utils.Gzip
import zio.*
import zio.compress.GzipCompressor
import zio.stream.*
import zio.test.*

import java.nio.charset.StandardCharsets

object GzipSpec extends ZIOSpecDefault:

  private val content = Chunk.fromArray("zio gzip".getBytes(StandardCharsets.UTF_8))

  def spec = suite("Gzip")(
    test("decompresses gzip with zio-streams-compress"):
      for
        compressed <- ZStream.fromChunk(content).via(GzipCompressor.compress).runCollect
        decoded    <- ZStream.fromChunk(compressed).via(Gzip.decompress).runCollect
      yield assertTrue(decoded == content)
    ,
    test("preserves npm's non-gzip fallback"):
      ZStream.fromChunk(content).via(Gzip.decompressOrIdentity).runCollect.map(decoded =>
        assertTrue(decoded == content)
      )
    ,
    test("strict decompression rejects non-gzip input"):
      ZStream.fromChunk(content).via(Gzip.decompress).runCollect.exit.map(exit => assertTrue(exit.isFailure))
  )
