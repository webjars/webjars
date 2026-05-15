package webjars.views

import webjars.utils.WebJars
import webjars.views.documentations.*
import zio.http.template2.*

object DocumentationPage:

  private def docTab(id: String, label: String, active: Boolean): Dom =
    li(className := "nav-item", role := "presentation",
      a(
        className                       := (if active then "nav-link active" else "nav-link"),
        href                            := s"#$id",
        Dom.attr("data-bs-toggle", "tab"),
        Dom.attr("data-bs-target", s"#$id"),
        role                            := "tab",
        Dom.attr("aria-controls", id),
        Dom.attr("aria-selected", if active then "true" else "false"),
        label,
      ),
    )

  private def docPane(id: String, content: Dom, active: Boolean): Dom =
    div(
      className := (if active then "tab-pane active" else "tab-pane"),
      Dom.attr("id", id),
      content,
    )

  // (id, label, content) for every documentation tab. The first entry is
  // rendered active by default.
  private val docs: Seq[(String, String, Dom)] = Seq(
    ("usage",       "Usage",            Usage()),
    ("play2",       "Play Framework 2", PlayFramework()),
    ("xitrum",      "Xitrum",           Xitrum()),
    ("servlet3",    "Servlet 3",        Servlet3()),
    ("servlet2",    "Servlet 2",        Servlet2()),
    ("jsf",         "JSF",              Jsf()),
    ("grails",      "Grails",           Grails()),
    ("dropwizard",  "Dropwizard",       Dropwizard()),
    ("springboot",  "Spring Boot",      SpringBoot()),
    ("springmvc",   "Spring MVC",       SpringMvc()),
    ("tapestry",    "Apache Tapestry",  Tapestry()),
    ("wicket",      "Apache Wicket",    Wicket()),
    ("pippo",       "Pippo",            Pippo()),
    ("ring",        "Ring (Clojure)",   Ring()),
    ("dandelion",   "Dandelion",        Dandelion()),
    ("vertx",       "Vert.x Web",       Vertx()),
    ("quarkus",     "Quarkus",          Quarkus()),
    ("ktor",        "Ktor",             Ktor()),
  )

  def apply(webJars: WebJars): Dom =
    val extraHead = script(Dom.boolAttr("defer"), src := "/assets/javascripts/docs.js")

    val tabs: Seq[Dom] = docs.zipWithIndex.map { case ((id, label, _), i) =>
      docTab(id, label, active = i == 0)
    }
    val panes: Seq[Dom] = docs.zipWithIndex.map { case ((id, _, content), i) =>
      docPane(id, content, active = i == 0)
    }

    MainLayout(webJars, "WebJars - Documentation", "/documentation", extraHead) {
      div(className := "container page-wrapper",
        h1("Documentation"),
        div(className := "row",
          aside(className := "col-lg-3 col-xl-2",
            div(
              Dom.attr("id", "sidebar-nav"),
              className := "offcanvas-lg offcanvas-end",
              tabindex  := "-1",
              Dom.attr("aria-labelledby", "docsSidebarOffcanvasLabel"),
              div(className := "offcanvas-header border-bottom",
                Dom.element("h5")(Dom.attr("id", "docsSidebarOffcanvasLabel"), className := "offcanvas-title", "Browse docs"),
                button(
                  `type`    := "button",
                  className := "btn-close",
                  Dom.attr("data-bs-dismiss", "offcanvas"),
                  Dom.attr("aria-label", "Close"),
                  Dom.attr("data-bs-target", "#sidebar-nav"),
                ),
              ),
              div(className := "offcanvas-body position-relative",
                nav(Dom.attr("aria-label", "Docs navigation"),
                  ul(
                    Dom.attr("id", "framework-tabs"),
                    className := "nav nav-pills flex-column",
                    role      := "tablist",
                    tabs,
                  ),
                ),
              ),
            ),
          ),
          div(className := "col col-lg-9 col-xl-10 tab-content", panes),
        ),
      )
    }
