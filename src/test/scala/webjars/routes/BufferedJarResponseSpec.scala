package webjars.routes

import webjars.utils.ServerError
import zio.*
import zio.http.*
import zio.stream.ZStream
import zio.test.*

object BufferedJarResponseSpec extends ZIOSpecDefault:

  def spec = suite("Buffered JAR response")(
    test("returns a complete body and Content-Length after consuming the stream") {
      val bytes = Chunk[Byte](0x50, 0x4b, 0x03, 0x04, 0x01, 0x02)
      val stream = ZStream.fromChunk(bytes.take(2)) ++ ZStream.fromChunk(bytes.drop(2))

      for
        response <- AppRoutes.bufferedJarResponse("example.jar", stream)
        body     <- response.body.asChunk
      yield assertTrue(
        response.status == Status.Ok,
        body == bytes,
        response.body.isComplete,
        response.headers.get(Header.ContentType).exists(_.renderedValue == "application/java-archive"),
        response.headers.get(Header.ContentLength).exists(_.length == bytes.length.toLong),
        response.headers.get(Header.ContentDisposition).nonEmpty,
      )
    },
    test("fails before producing a response when the stream fails") {
      val failure = RuntimeException("archive stream failed")
      val stream = ZStream.fromChunk(Chunk[Byte](0x50, 0x4b)) ++ ZStream.fail(failure)

      AppRoutes.bufferedJarResponse("broken.jar", stream).exit.map: exit =>
        assertTrue(
          exit.isFailure,
          exit.causeOption.flatMap(_.failureOption).contains(failure),
        )
    },
    test("returns a safe plain-text message without the upstream response body") {
      val technicalMessage = "API rate limit exceeded for 192.0.2.1 with secret-token"
      val error = ServerError(technicalMessage, 429)

      for
        response <- AppRoutes.createFailureResponse(Status.BadRequest, "/create", error)
        body     <- response.body.asString
      yield assertTrue(
        response.status == Status.BadRequest,
        response.headers.get(Header.ContentType).exists(_.renderedValue == "text/plain"),
        body == "An upstream service is temporarily rate limiting requests. Please wait a few minutes and try again.",
        !body.contains(technicalMessage),
        !body.contains("secret-token"),
      )
    },
  )
