package webjars.views

import webjars.models.WebJar
import java.net.URLEncoder

object AllPage:
  def apply(webjarsOrError: Either[Iterable[WebJar], String]): String =
    val rows = webjarsOrError match
      case Right(error) =>
        s"""<tr>
                                <td colspan="4">$error</td>
                            </tr>"""
      case Left(webjars) =>
        if webjars.isEmpty then
          """<tr>
                                <td colspan="4">No WebJars were found.</td>
                            </tr>"""
        else
          webjars.map { webjar =>
            s"""<tr>
                                    <td>
                                        <a href="${webjar.sourceUrl}"><strong>${webjar.name}</strong></a>
                                    </td>
                                    <td>
                                        ${webjar.groupId}
                                    </td>
                                    <td>
                                        ${webjar.artifactId}
                                    </td>
                                    <td>
                                        ${webjar.versions.map { version =>
                                          s"""<a href="/listfiles/${webjar.groupId}/${webjar.artifactId}/${URLEncoder.encode(version.number, "UTF-8")}">${version.number}</a>"""
                                        }.mkString("\n                                        ")}
                                    </td>
                                </tr>"""
          }.mkString("\n")

    MainLayout("WebJars - Web Libraries in Jars", "/all") {
      s"""
    <div class="container page-wrapper">
        <h1>All Deployed WebJars</h1>

        <div class="table-responsive">
            <table class="table table-striped table-hover align-middle">
                <thead>
                    <tr>
                        <th class="table-name">Name</th>
                        <th>GroupId</th>
                        <th>ArtifactId</th>
                        <th>Versions</th>
                    </tr>
                </thead>
                <tbody>
                    $rows
                </tbody>
            </table>
        </div>
    </div>"""
    }
