package utils

import play.api.test.PlaySpecification

object WebJarsFileServiceSpec extends PlaySpecification with GlobalApplication {
  implicit lazy val webJarsFileService: WebJarsFileService = application.injector.instanceOf[WebJarsFileService]

  "getFileList" should {
    "work" in {
      val fileList = await(webJarsFileService.getFileList("org.webjars", "jquery", "3.6.4"))
      fileList must contain("META-INF/resources/webjars/jquery/3.6.4/jquery.js")
    }
  }

  "getNumFiles" should {
    "work" in {
      val numFiles = await(webJarsFileService.getNumFiles("org.webjars", "jquery", "3.6.4"))
      numFiles mustEqual 7
    }
  }
}
