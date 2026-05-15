package webjars.views

import webjars.generated.WebJars.Artifact.*
import webjars.utils.WebJars
import webjars.views.partials.{Footer, Icons, Navbar}
import zio.http.template2.*

object MainLayout:

  def apply(webJars: WebJars, title: String, currentPath: String = "/", extraHead: Dom = Dom.empty)(content: Dom): Dom =
    html(Dom.attr("lang", "en"), Dom.attr("data-bs-theme", "auto"),
      head(
        meta(charset := "utf-8"),
        meta(Dom.attr("http-equiv", "X-UA-Compatible"), Dom.attr("content", "IE=edge")),
        meta(name := "viewport", Dom.attr("content", "width=device-width, initial-scale=1")),
        Dom.element("title")(title),
        link(rel := "shortcut icon", href := "/favicon.ico"),
        link(rel := "icon", `type` := "image/png", href := "/assets/logo.png"),

        // Google fonts
        link(rel := "preconnect", href := "https://fonts.googleapis.com"),
        link(rel := "preconnect", href := "https://fonts.gstatic.com", Dom.boolAttr("crossorigin")),
        link(href := "https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap", rel := "stylesheet"),

        // Styles — main.css is generated from main.scss by sbt-sass and includes Bootstrap
        link(rel := "stylesheet", href := "/assets/main.css"),
        link(rel := "stylesheet", href := webJars.url(select2, "css/select2.min.css")),
        link(rel := "stylesheet", href := webJars.url(`select2-bootstrap-5-theme`, "dist/select2-bootstrap-5-theme.min.css")),
        link(rel := "stylesheet", href := webJars.url(highlightjs, "styles/atom-one-dark.min.css")),

        // Scripts
        script(src := webJars.url(jquery, "jquery.min.js")),
        script(src := webJars.url(bootstrap, "dist/js/bootstrap.bundle.min.js")),
        script(src := webJars.url(select2, "js/select2.full.min.js")),
        script(src := webJars.url(highlightjs, "highlight.min.js")),
        script(Dom.raw("hljs.highlightAll();")),
        script(src := "/assets/javascripts/color-modes.js"),

        // Google tag (gtag.js)
        script(Dom.boolAttr("async"), src := "https://www.googletagmanager.com/gtag/js?id=G-EEV2YQX882"),
        script(Dom.raw(
          """
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'G-EEV2YQX882');
          """
        )),
        extraHead,
      ),
      body(
        Icons(),
        div(className := "min-vh-100 h-full d-flex flex-column justify-content-between",
          Navbar(currentPath),
          div(className := "flex-grow-1", content),
          Footer(),
        ),
      ),
    )
