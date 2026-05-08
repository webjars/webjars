package webjars.views

import webjars.views.documentations.*

object DocumentationPage:
  def apply(): String =
    val extraHead = """<script defer src="/assets/javascripts/docs.js"></script>"""

    MainLayout("WebJars - Documentation", "/documentation", extraHead) {
      s"""
<div class="container page-wrapper">
    <h1>Documentation</h1>

    <div class="row">

        <aside class="col-lg-3 col-xl-2">
            <div id="sidebar-nav" class="offcanvas-lg offcanvas-end" tabindex="-1" aria-labelledby="docsSidebarOffcanvasLabel">
                <div class="offcanvas-header border-bottom">
                    <h5 id="docsSidebarOffcanvasLabel" class="offcanvas-title">Browse docs</h5>
                    <button type="button" class="btn-close" data-bs-dismiss="offcanvas" aria-label="Close" data-bs-target="#sidebar-nav"></button>
                </div>

                <div class="offcanvas-body position-relative">
                    <nav aria-label="Docs navigation">
                        <ul id="framework-tabs" class="nav nav-pills flex-column" role="tablist">
                            ${docTab("usage", "Usage", true)}
                            ${docTab("play2", "Play Framework 2")}
                            ${docTab("xitrum", "Xitrum")}
                            ${docTab("servlet3", "Servlet 3")}
                            ${docTab("servlet2", "Servlet 2")}
                            ${docTab("jsf", "JSF")}
                            ${docTab("grails", "Grails")}
                            ${docTab("dropwizard", "Dropwizard")}
                            ${docTab("springboot", "Spring Boot")}
                            ${docTab("springmvc", "Spring MVC")}
                            ${docTab("tapestry", "Apache Tapestry")}
                            ${docTab("wicket", "Apache Wicket")}
                            ${docTab("pippo", "Pippo")}
                            ${docTab("ring", "Ring (Clojure)")}
                            ${docTab("dandelion", "Dandelion")}
                            ${docTab("vertx", "Vert.x Web")}
                            ${docTab("quarkus", "Quarkus")}
                            ${docTab("ktor", "Ktor")}
                        </ul>
                    </nav>
                </div>
            </div>
        </aside>

        <div class="col col-lg-9 col-xl-10 tab-content">
            ${docPane("usage", Usage(), true)}
            ${docPane("play2", PlayFramework())}
            ${docPane("xitrum", Xitrum())}
            ${docPane("servlet3", Servlet3())}
            ${docPane("servlet2", Servlet2())}
            ${docPane("jsf", Jsf())}
            ${docPane("grails", Grails())}
            ${docPane("dropwizard", Dropwizard())}
            ${docPane("springboot", SpringBoot())}
            ${docPane("springmvc", SpringMvc())}
            ${docPane("tapestry", Tapestry())}
            ${docPane("wicket", Wicket())}
            ${docPane("pippo", Pippo())}
            ${docPane("ring", Ring())}
            ${docPane("dandelion", Dandelion())}
            ${docPane("vertx", Vertx())}
            ${docPane("quarkus", Quarkus())}
            ${docPane("ktor", Ktor())}
        </div>

    </div>

</div>"""
    }

  private def docTab(id: String, label: String, active: Boolean = false): String =
    val activeClass = if active then " active" else ""
    val selected = if active then "true" else "false"
    s"""<li class="nav-item" role="presentation">
                                <a class="nav-link$activeClass" href="#$id" data-bs-toggle="tab" data-bs-target="#$id" role="tab" aria-controls="$id" aria-selected="$selected">
                                    $label
                                </a>
                            </li>"""

  private def docPane(id: String, content: String, active: Boolean = false): String =
    val activeClass = if active then " active" else ""
    s"""<div class="tab-pane$activeClass" id="$id">
                $content
            </div>"""
