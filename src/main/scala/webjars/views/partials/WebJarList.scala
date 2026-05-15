package webjars.views.partials

import webjars.models.WebJar
import zio.http.template2.*

import java.net.URLEncoder

object WebJarList:

  private def displayNumFiles(maybeNumFiles: Option[Int]): String =
    maybeNumFiles.fold("List")(_.toString)

  private def errorRow(error: String): Dom =
    tr(td(Dom.attr("colspan", "4"), error))

  private def emptyRow: Dom =
    tr(td(Dom.attr("colspan", "4"), "No WebJars were found."))

  private def webjarRow(webjar: WebJar): Dom =
    val versionOptions: Seq[Dom] = webjar.versions.map: v =>
      option(Dom.attr("data-numfiles", displayNumFiles(v.numFiles)), v.number)
    val firstVersion = webjar.versions.head
    val encodedVersion = URLEncoder.encode(firstVersion.number, "UTF-8")
    tr(
      Dom.attr("data-group", webjar.groupId.toString),
      Dom.attr("data-artifact", webjar.artifactId.toString),
      td(a(href := webjar.sourceUrl, strong(webjar.name))),
      td(
        div(className := "d-flex align-items-center gap-2",
          form(role := "form",
            select(
              className     := "form-select form-select-sm versions",
              Dom.attr("onchange", "changeVersion(event)"),
              name          := s"${webjar.groupId}:${webjar.artifactId}",
              versionOptions,
            ),
          ),
          button(
            Dom.attr("title", "Create a new version"),
            `type`        := "button",
            className     := "btn btn-primary d-flex align-items-center justify-content-center",
            Dom.attr("data-bs-toggle", "modal"),
            Dom.attr("data-bs-target", "#newWebJarModal"),
            Dom.attr("data-group-id", webjar.groupId.toString),
            Dom.attr("data-artifact-id", webjar.artifactId.toString),
            Dom.attr("data-name", webjar.name),
            Dom.raw("""<svg class="bi bi-plus-lg"><use href="#plus-lg"></use></svg>"""),
          ),
        ),
      ),
      td(
        div(className := "build-instructions",
          pre(s""""${webjar.groupId}" % "${webjar.artifactId}" % "${firstVersion.number}""""),
        ),
      ),
      td(
        div(className := s"files text-end ${webjar.artifactId}",
          a(
            href      := s"/listfiles/${webjar.groupId}/${webjar.artifactId}/$encodedVersion",
            className := "file-list-link",
            s"${displayNumFiles(firstVersion.numFiles)} Files",
          ),
        ),
      ),
    )

  def apply(webjarsOrError: Either[Iterable[WebJar], String]): Dom =
    val rows: Seq[Dom] = webjarsOrError match
      case Right(error)  => Seq(errorRow(error))
      case Left(webjars) =>
        if webjars.isEmpty then Seq(emptyRow)
        else webjars.toSeq.map(webjarRow)

    table(className := "table table-striped table-hover align-middle",
      thead(
        tr(
          th(className := "table-name", "Name"),
          th(className := "table-versions", "Versions"),
          th("Dependency"),
          th(className := "table-files text-end", "Files"),
        ),
      ),
      tbody(rows),
    )
