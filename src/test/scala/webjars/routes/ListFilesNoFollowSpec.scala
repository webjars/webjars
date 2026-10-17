package webjars.routes

import webjars.views.FileListPage
import zio.http.*
import zio.test.*

object ListFilesNoFollowSpec extends ZIOSpecDefault:

  def spec = suite("listfiles crawler hints")(
    test("file links are marked nofollow"):
      val html = FileListPage(
        "org.webjars", "jquery", "3.7.1",
        List("META-INF/resources/webjars/jquery/3.7.1/jquery.js"),
      ).render
      assertTrue(
        html.contains("""href="//cdn.jsdelivr.net/webjars/org.webjars/jquery/3.7.1/jquery.js""""),
        html.contains("""rel="nofollow noopener""""),
      )
    ,
    test("noIndex adds X-Robots-Tag: noindex, nofollow"):
      val response = AppRoutes.noIndex(Response.ok)
      assertTrue(response.rawHeader("X-Robots-Tag").contains("noindex, nofollow"))
    ,
  )
