package webjars.views

import webjars.models.WebJar
import webjars.utils.WebJars
import webjars.views.partials.{FileListModal, NewWebJarModal}
import webjars.views.sections.{Hero, Popular}
import zio.http.template2.*

object IndexPage:
  def apply(webJars: WebJars, webjarsOrError: Either[Iterable[WebJar], String]): Dom =
    val extraHead = script(Dom.boolAttr("defer"), src := "/assets/javascripts/index.js")

    MainLayout(webJars, "WebJars - Web Libraries in Jars", "/", extraHead) {
      Dom.fragment(
        div(className := "home-bg",
          Hero(),
          Popular(webjarsOrError),
        ),
        FileListModal(),
        NewWebJarModal(),
      )
    }
