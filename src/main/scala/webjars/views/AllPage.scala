package webjars.views

import webjars.models.WebJar
import webjars.utils.WebJars
import zio.http.template2.*

import java.net.URLEncoder

object AllPage:

  private def errorRow(error: String): Dom =
    tr(td(Dom.attr("colspan", "4"), error))

  private val emptyRow: Dom =
    tr(td(Dom.attr("colspan", "4"), "No WebJars were found."))

  private def webjarRow(webjar: WebJar): Dom =
    val versionLinks: Seq[Dom] = webjar.versions.map: v =>
      a(
        href := s"/listfiles/${webjar.groupId}/${webjar.artifactId}/${URLEncoder.encode(v.number, "UTF-8")}",
        v.number,
      )
    tr(
      td(a(href := webjar.sourceUrl, strong(webjar.name))),
      td(webjar.groupId.toString),
      td(webjar.artifactId.toString),
      td(versionLinks),
    )

  def apply(webJars: WebJars, webjarsOrError: Either[Iterable[WebJar], String]): Dom =
    val rows: Seq[Dom] = webjarsOrError match
      case Right(error)  => Seq(errorRow(error))
      case Left(webjars) =>
        if webjars.isEmpty then Seq(emptyRow) else webjars.toSeq.map(webjarRow)

    MainLayout(webJars, "WebJars - Web Libraries in Jars", "/all") {
      div(className := "container page-wrapper",
        h1("All Deployed WebJars"),
        div(className := "table-responsive",
          table(className := "table table-striped table-hover align-middle",
            thead(
              tr(
                th(className := "table-name", "Name"),
                th("GroupId"),
                th("ArtifactId"),
                th("Versions"),
              ),
            ),
            tbody(rows),
          ),
        ),
      )
    }
