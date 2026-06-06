package webjars

import webjars.utils.*
import zio.test.*

import java.io.FileNotFoundException
import java.net.{ConnectException, UnknownHostException}
import java.util.concurrent.TimeoutException

object DeployFailureSpec extends ZIOSpecDefault:

  def spec = suite("DeployFailure.classify")(
    suite("Systemic — file an issue")(
      test("LicenseNotFoundException") {
        assertTrue(DeployFailure.classify(LicenseNotFoundException("nope")).isInstanceOf[DeployFailure.Systemic])
      },
      test("NoValidLicenses") {
        assertTrue(DeployFailure.classify(NoValidLicenses()).isInstanceOf[DeployFailure.Systemic])
      },
      test("MissingMetadataException") {
        assertTrue(DeployFailure.classify(MissingMetadataException("{}", Seq("name"))).isInstanceOf[DeployFailure.Systemic])
      },
      test("UnauthorizedError") {
        assertTrue(DeployFailure.classify(UnauthorizedError("nope")).isInstanceOf[DeployFailure.Systemic])
      },
      test("ServerError 401") {
        assertTrue(DeployFailure.classify(ServerError("nope", 401)).isInstanceOf[DeployFailure.Systemic])
      },
      test("ServerError 403") {
        assertTrue(DeployFailure.classify(ServerError("nope", 403)).isInstanceOf[DeployFailure.Systemic])
      },
      test("FileNotFoundException — missing file in upstream archive") {
        assertTrue(DeployFailure.classify(new FileNotFoundException("package.json")).isInstanceOf[DeployFailure.Systemic])
      },
    ),
    suite("UserInput — show but don't file")(
      test("ServerError 404 — package not found upstream") {
        assertTrue(DeployFailure.classify(ServerError("not found", 404)).isInstanceOf[DeployFailure.UserInput])
      },
      test("IllegalStateException — already deployed") {
        assertTrue(DeployFailure.classify(new IllegalStateException("already deployed")).isInstanceOf[DeployFailure.UserInput])
      },
    ),
    suite("Transient — show but don't file")(
      test("ServerError 429 — rate limit") {
        assertTrue(DeployFailure.classify(ServerError("rate limit", 429)).isInstanceOf[DeployFailure.Transient])
      },
      test("ServerError 500") {
        assertTrue(DeployFailure.classify(ServerError("oops", 500)).isInstanceOf[DeployFailure.Transient])
      },
      test("ServerError 502") {
        assertTrue(DeployFailure.classify(ServerError("oops", 502)).isInstanceOf[DeployFailure.Transient])
      },
      test("ServerError with non-canonical status — fall through to default") {
        // 418 isn't 404, 401-3, 429, or 5xx → default ServerError branch is Transient.
        assertTrue(DeployFailure.classify(ServerError("teapot", 418)).isInstanceOf[DeployFailure.Transient])
      },
      test("TimeoutException") {
        assertTrue(DeployFailure.classify(new TimeoutException("slow")).isInstanceOf[DeployFailure.Transient])
      },
      test("ConnectException") {
        assertTrue(DeployFailure.classify(new ConnectException("refused")).isInstanceOf[DeployFailure.Transient])
      },
      test("UnknownHostException") {
        assertTrue(DeployFailure.classify(new UnknownHostException("x")).isInstanceOf[DeployFailure.Transient])
      },
      test("Unknown plain Exception defaults to Transient (don't spam tracker)") {
        assertTrue(DeployFailure.classify(new Exception("mystery")).isInstanceOf[DeployFailure.Transient])
      },
    ),
    suite("shouldFileIssue")(
      test("only Systemic returns true") {
        val systemic = DeployFailure.Systemic(LicenseNotFoundException("x"))
        val transient = DeployFailure.Transient(new TimeoutException("x"))
        val userInput = DeployFailure.UserInput(ServerError("x", 404))
        assertTrue(
          DeployFailure.shouldFileIssue(systemic),
          !DeployFailure.shouldFileIssue(transient),
          !DeployFailure.shouldFileIssue(userInput),
        )
      },
    ),
    suite("tag")(
      test("matches the wire format used by the SSE log") {
        val systemic = DeployFailure.Systemic(LicenseNotFoundException("x"))
        val transient = DeployFailure.Transient(new TimeoutException("x"))
        val userInput = DeployFailure.UserInput(ServerError("x", 404))
        assertTrue(
          DeployFailure.tag(systemic) == "systemic",
          DeployFailure.tag(transient) == "transient",
          DeployFailure.tag(userInput) == "user-input",
        )
      },
    ),
    suite("message")(
      test("uses cause.getMessage when present") {
        val failure = DeployFailure.classify(new Exception("a clear message"))
        assertTrue(failure.message == "a clear message")
      },
      test("falls back to class simple name when getMessage is null/empty") {
        val failure = DeployFailure.classify(new Exception())
        assertTrue(failure.message == "Exception")
      },
    ),
  )
