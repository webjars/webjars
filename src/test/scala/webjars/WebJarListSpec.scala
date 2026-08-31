package webjars

import webjars.models.{WebJar, WebJarVersion}
import webjars.views.partials.WebJarList
import zio.test.*

object WebJarListSpec extends ZIOSpecDefault:

  private val version = Seq(WebJarVersion("1.0.0"))

  def spec = suite("WebJarList")(
    test("NPM plus button carries explicit modal type and package name") {
      val webJar = WebJar(
        "org.webjars.npm",
        "scope__package",
        "@scope/package",
        "https://example.test/npm",
        version,
      )
      val html = WebJarList(Left(Seq(webJar))).render

      assertTrue(
        html.contains("""data-webjar-type="npm"""),
        html.contains("""data-group-id="org.webjars.npm"""),
        html.contains("""data-name="@scope/package"""),
        html.contains("""data-artifact-id="scope__package"""),
      )
    },
    test("Classic plus button carries explicit modal type and artifact ID") {
      val webJar = WebJar(
        "org.webjars",
        "jquery",
        "jQuery",
        "https://example.test/classic",
        version,
      )
      val html = WebJarList(Left(Seq(webJar))).render

      assertTrue(
        html.contains("""data-webjar-type="classic"""),
        html.contains("""data-group-id="org.webjars"""),
        html.contains("""data-name="jQuery"""),
        html.contains("""data-artifact-id="jquery"""),
      )
    },
  )
