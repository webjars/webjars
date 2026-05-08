package webjars.views

import webjars.models.WebJar
import webjars.views.partials.{FileListModal, NewWebJarModal}
import webjars.views.sections.{Hero, Popular}

object IndexPage:
  def apply(webjarsOrError: Either[Iterable[WebJar], String]): String =
    val extraHead = """<script defer src="/assets/javascripts/index.js"></script>
        <script src="/webjars/jquery.typewatch/2.1.0/jquery.typewatch.js"></script>"""

    MainLayout("WebJars - Web Libraries in Jars", "/", extraHead) {
      s"""<!-- Content -->
    <div class="home-bg">
        <!-- Hero -->
        ${Hero()}

        <!-- Popular WebJars -->
        ${Popular(webjarsOrError)}
    </div>

    <!-- Modals -->
    ${FileListModal()}

    ${NewWebJarModal()}"""
    }
