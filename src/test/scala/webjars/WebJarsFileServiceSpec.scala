package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.{WebJarsFileService, WebJarsFileServiceLive}
import webjars.TestInfrastructure.testConfig
import zio.*
import zio.http.Client
import zio.test.*

object WebJarsFileServiceSpec extends ZIOSpecDefault:

  def spec = suite("WebJarsFileService")(
    test("getFileList works") {
      for
        client <- ZIO.service[Client]
        webJarsFileService = WebJarsFileServiceLive(client, testConfig)
        gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"), MavenCentral.Version("3.6.4"))
        fileList <- ZIO.scoped(webJarsFileService.getFileList(gav))
      yield assertTrue(fileList.contains("META-INF/resources/webjars/jquery/3.6.4/jquery.js"))
    } @@ TestAspect.withLiveClock,
    test("getNumFiles works") {
      for
        client <- ZIO.service[Client]
        webJarsFileService = WebJarsFileServiceLive(client, testConfig)
        gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"), MavenCentral.Version("3.6.4"))
        numFiles <- ZIO.scoped(webJarsFileService.getNumFiles(gav))
      yield assertTrue(numFiles == 7)
    } @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(30.seconds)
