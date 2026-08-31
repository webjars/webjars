package webjars

import webjars.views.partials.NewWebJarModal
import zio.test.*

object NewWebJarModalSpec extends ZIOSpecDefault:

  def spec = suite("NewWebJarModal")(
    test("requires a WebJar type before enabling the name field") {
      val html = NewWebJarModal().render

      assertTrue(
        !html.contains("""value="npm" checked"""),
        !html.contains("""value="classic" checked"""),
        html.contains("""placeholder="Package Name or Git Repo URL" disabled>"""),
      )
    },
    test("includes a hidden accessible deployment error alert") {
      val html = NewWebJarModal().render

      assertTrue(
        html.contains("""id="deployError" class="alert alert-danger d-none mb-3" role="alert" aria-live="assertive"""),
        html.contains("""id="deployErrorTitle"""),
        html.contains("""id="deployErrorMessage"""),
        html.contains("""id="deployErrorTrackingUrl"""),
      )
    },
    test("associates type labels with unique modal radio IDs") {
      val html = NewWebJarModal().render

      assertTrue(
        html.contains("""id="newWebJarTypeNpm"""),
        html.contains("""for="newWebJarTypeNpm">NPM</label>"""),
        html.contains("""id="newWebJarTypeClassic"""),
        html.contains("""for="newWebJarTypeClassic">Classic</label>"""),
        !html.contains("""id="npm" class="form-check-input" type="radio"""),
        !html.contains("""id="classic" class="form-check-input" type="radio"""),
      )
    },
  )
