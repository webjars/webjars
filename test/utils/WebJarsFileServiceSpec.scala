package utils

import zio.test._
import zio.direct._
import zio.http.Client

object WebJarsFileServiceSpec extends ZIOSpecDefault {

  def spec = suite("WebJarsFileService")(
    test("getFileList") {
      defer {
        val fileList = WebJarsFileService.getFileList("org.webjars", "jquery", "3.6.4").run
        assertTrue(fileList.contains("META-INF/resources/webjars/jquery/3.6.4/jquery.js"))
      }
    },
    test("getNumFiles") {
      defer {
        val fileList = WebJarsFileService.getNumFiles("org.webjars", "jquery", "3.6.4").run
        assertTrue(fileList == 7)
      }
    },
  ).provide(Client.default)

}
