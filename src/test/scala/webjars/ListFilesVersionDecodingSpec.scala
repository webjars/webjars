package webjars

import zio.*
import zio.http.*
import zio.test.*

import java.net.URLDecoder

/**
 * Reproduces https://www.webjars.org/listfiles/org.webjars/jquery-ui/1.14.2%2B1
 * returning 404.
 *
 * `AppRoutes`'s `/listfiles` routes match the version segment with
 * `string("version")`, whose value zio-http has already percent-decoded
 * (`%2B` -> `+`) by the time the handler sees it. `AppRoutes.handleListFiles`
 * is then called with `URLDecoder.decode(version, "UTF-8")` -- a *second*
 * decode pass. `URLDecoder` additionally folds `+` into a space (that's
 * `application/x-www-form-urlencoded` semantics, not path-segment
 * semantics), so a version like `1.14.2+1` becomes `1.14.2 1`, which then
 * fails to resolve on Maven Central.
 *
 * This spec isolates the routing/decoding layer (same `PathCodec` pattern
 * as the real `/listfiles` route) from the rest of `AppRoutes`'s dependency
 * graph (Redis, MavenCentralWebJars, SearchIndex, etc.), since none of
 * those participate in the bug.
 */
object ListFilesVersionDecodingSpec extends ZIOSpecDefault:

  private def routeWithVersionHandling(handleVersion: String => String): Routes[Any, Response] =
    Routes(
      Method.GET / "listfiles" / string("artifactId") / string("version") ->
        handler { (artifactId: String, version: String, _: Request) =>
          Response.text(handleVersion(version))
        }
    )

  def spec = suite("listfiles version decoding")(
    test("zio-http already percent-decodes the version path segment") {
      val request = Request.get(URL.decode("/listfiles/jquery-ui/1.14.2%2B1").toOption.get)
      for
        response <- routeWithVersionHandling(identity).runZIO(request)
        body     <- response.body.asString
      yield assertTrue(body == "1.14.2+1")
    },

    test("a version containing a literal '+' must survive the /listfiles handler intact") {
      // This is the behavior AppRoutes.handleListFiles must produce.
      // Before the fix, AppRoutes ran the already-decoded segment back
      // through URLDecoder.decode, which turns '+' into ' '.
      val request = Request.get(URL.decode("/listfiles/jquery-ui/1.14.2%2B1").toOption.get)
      for
        response <- routeWithVersionHandling(identity).runZIO(request)
        body     <- response.body.asString
      yield assertTrue(body == "1.14.2+1", !body.contains(" "))
    },

    test("demonstrates the bug: re-decoding the already-decoded segment corrupts '+' into a space") {
      val request = Request.get(URL.decode("/listfiles/jquery-ui/1.14.2%2B1").toOption.get)
      for
        response <- routeWithVersionHandling(v => URLDecoder.decode(v, "UTF-8")).runZIO(request)
        body     <- response.body.asString
      yield assertTrue(body == "1.14.2 1")
    },
  )
