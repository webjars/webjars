package utils

import com.jamesward.zio_mavencentral.MavenCentral
import play.api.test.PlaySpecification

object WebJarsFileServiceSpec extends PlaySpecification with GlobalApplication {
  implicit lazy val webJarsFileService: WebJarsFileService = application.injector.instanceOf[WebJarsFileService]

  "getFileList" should {
    "work" in {
      val gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"), MavenCentral.Version("3.6.4"))
      val fileList = await(webJarsFileService.getFileList(gav))
      fileList must contain("META-INF/resources/webjars/jquery/3.6.4/jquery.js")
    }
  }

  "getNumFiles" should {
    "work" in {
      val gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"), MavenCentral.Version("3.6.4"))
      val numFiles = await(webJarsFileService.getNumFiles(gav))
      numFiles mustEqual 7
    }
  }
}
