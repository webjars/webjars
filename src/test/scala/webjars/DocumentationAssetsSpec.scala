package webjars

import webjars.generated.WebJars as Gen
import webjars.utils.WebJars
import webjars.views.{DocumentationPage, MainLayout}
import zio.http.URL
import zio.http.template2.Dom
import zio.test.*

object DocumentationAssetsSpec extends ZIOSpecDefault:

  private object LocalWebJars extends WebJars:
    override def url(artifact: Gen.Artifact, path: String): URL =
      URL.decode(Gen.localUrl(artifact, path)).toOption.get

  def spec = suite("Documentation assets")(
    test("loads Highlight.js only on the documentation page") {
      val documentation = DocumentationPage(LocalWebJars).render
      val genericPage   = MainLayout(LocalWebJars, "Generic")(Dom.raw("<div>content</div>")).render

      assertTrue(
        documentation.contains("/webjars/highlightjs/11.11.1/styles/atom-one-dark.min.css"),
        documentation.contains("/webjars/highlightjs/11.11.1/highlight.min.js"),
        documentation.contains("hljs.highlightAll();"),
        !genericPage.contains("highlightjs"),
        !genericPage.contains("hljs.highlightAll();"),
      )
    },
  )
