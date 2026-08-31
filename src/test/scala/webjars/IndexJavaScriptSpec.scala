package webjars

import zio.test.*

import scala.io.Source
import scala.util.Using

object IndexJavaScriptSpec extends ZIOSpecDefault:

  private def indexJavaScript: String =
    Using.resource(Source.fromResource("public/javascripts/index.js"))(_.mkString)

  def spec = suite("index.js")(
    test("does not request package existence for an empty or whitespace name") {
      val javascript = indexJavaScript
      val functionStart = javascript.indexOf("function checkPackageName(packageName)")
      val emptyGuard = javascript.indexOf("if (packageName.length === 0)", functionStart)
      val earlyReturn = javascript.indexOf("return;", emptyGuard)
      val existsRequest = javascript.indexOf("/exists?", functionStart)

      assertTrue(
        functionStart >= 0,
        javascript.indexOf(".trim()", functionStart) < emptyGuard,
        emptyGuard > functionStart,
        earlyReturn > emptyGuard,
        existsRequest > earlyReturn,
      )
    },
  )
