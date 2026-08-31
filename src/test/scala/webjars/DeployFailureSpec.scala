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
    suite("userMessage")(
      test("explains missing packages without exposing the backend response") {
        val failure = DeployFailure.classify(ServerError("raw registry response", 404))
        assertTrue(
          failure.userMessage == "The requested package or version could not be found. Check the package name and version, then try again.",
          !failure.userMessage.contains("raw registry response"),
        )
      },
      test("explains rate limiting") {
        val failure = DeployFailure.classify(ServerError("raw rate-limit payload", 429))
        assertTrue(
          failure.userMessage == "An upstream service is temporarily rate limiting requests. Please wait a few minutes and try again.",
          !failure.userMessage.contains("raw rate-limit payload"),
        )
      },
      test("explains license failures without exposing metadata details") {
        val failure = DeployFailure.classify(LicenseNotFoundException("raw package metadata"))
        assertTrue(
          failure.userMessage == "A valid license could not be determined from the package metadata. A maintainer may need to review this package.",
          !failure.userMessage.contains("raw package metadata"),
        )
      },
    ),
    suite("createUserMessage")(
      test("uses create wording and does not expose technical causes") {
        val userInput = DeployFailure.classify(new IllegalStateException("raw user detail"))
        val transient = DeployFailure.classify(new Exception("raw transient detail"))
        val systemic = DeployFailure.classify(LicenseNotFoundException("raw license detail"))

        assertTrue(
          DeployFailure.createUserMessage(userInput) == "The requested WebJar could not be created. Check the package name and version, then try again.",
          DeployFailure.createUserMessage(transient) == "A required upstream service is temporarily unavailable. Please try creating the WebJar again in a few minutes.",
          DeployFailure.createUserMessage(systemic) == "A valid license could not be determined from the package metadata. A maintainer may need to review this package.",
          !DeployFailure.createUserMessage(userInput).contains("raw user detail"),
          !DeployFailure.createUserMessage(transient).contains("raw transient detail"),
          !DeployFailure.createUserMessage(systemic).contains("raw license detail"),
        )
      },
    ),
  )
