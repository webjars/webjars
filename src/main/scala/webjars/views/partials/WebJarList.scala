package webjars.views.partials

import webjars.models.{WebJar, WebJarVersion}
import java.net.URLEncoder

object WebJarList:

  private def displayNumFiles(maybeNumFiles: Option[Int]): String =
    maybeNumFiles.fold("List")(_.toString)

  def apply(webjarsOrError: Either[Iterable[WebJar], String]): String =
    val rows = webjarsOrError match
      case Right(error) =>
        s"""<tr>
                    <td colspan="4">
                        $error
                    </td>
                </tr>"""
      case Left(webjars) =>
        if webjars.isEmpty then
          """<tr>
                    <td colspan="4">
                        No WebJars were found.
                    </td>
                </tr>"""
        else
          webjars.map { webjar =>
            s"""<tr data-group="${webjar.groupId}" data-artifact="${webjar.artifactId}">
                        <td>
                            <a href="${webjar.sourceUrl}"><strong>${webjar.name}</strong></a>
                        </td>
                        <td>
                            <div class="d-flex align-items-center gap-2">
                                <form role="form">
                                    <select class="form-select form-select-sm versions" onchange="changeVersion(event)" name="${webjar.groupId}:${webjar.artifactId}">
                                    ${webjar.versions.map { version =>
                                      s"""<option data-numfiles="${displayNumFiles(version.numFiles)}">${version.number}</option>"""
                                    }.mkString("\n                                    ")}
                                    </select>
                                </form>
                                <button title="Create a new version" type="button" class="btn btn-primary d-flex align-items-center justify-content-center" data-bs-toggle="modal" data-bs-target="#newWebJarModal" data-group-id="${webjar.groupId}" data-artifact-id="${webjar.artifactId}" data-name="${webjar.name}">
                                    <svg class="bi bi-plus-lg"><use href="#plus-lg"></use></svg>
                                </button>
                            </div>
                        </td>
                        <td>
                            <div class="build-instructions">
                                <pre>"${webjar.groupId}" % "${webjar.artifactId}" % "${webjar.versions.head.number}"</pre>
                            </div>
                        </td>
                        <td>
                            <div class="files text-end ${webjar.artifactId}">
                                <a href="/listfiles/${webjar.groupId}/${webjar.artifactId}/${URLEncoder.encode(webjar.versions.head.number, "UTF-8")}" class="file-list-link">
                                ${displayNumFiles(webjar.versions.head.numFiles)} Files
                                </a>
                            </div>
                        </td>
                    </tr>"""
          }.mkString("\n")

    s"""<table class="table table-striped table-hover align-middle">
    <thead>
        <tr>
            <th class="table-name">Name</th>
            <th class="table-versions">Versions</th>
            <th>Dependency</th>
            <th class="table-files text-end">Files</th>
        </tr>
    </thead>

    <tbody>
        $rows
    </tbody>
</table>"""
