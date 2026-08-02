package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.{WebJarsFileService, WebJarsFileServiceLive}
import zio.*
import zio.http.*
import zio.test.*

object WebJarsFileServiceSpec extends ZIOSpecDefault:

  private val gav = MavenCentral.GroupArtifactVersion(
    MavenCentral.GroupId("org.webjars"),
    MavenCentral.ArtifactId("jquery"),
    MavenCentral.Version("3.6.4"),
  )

  private val expectedFileList = """["META-INF/resources/webjars/jquery/3.6.4/jquery.js"]"""

  private val stubRoutes: Routes[Any, Response] = Routes(
    Method.GET / "listfiles" / string("groupId") / string("artifactId") / string("version") ->
      handler { (groupId: String, artifactId: String, version: String, _: Request) =>
        if groupId == "org.webjars" && artifactId == "jquery" && version == "3.6.4" then
          Response.json(expectedFileList)
        else
          Response.status(Status.NotFound)
      },
    Method.GET / "numfiles" / string("groupId") / string("artifactId") / string("version") ->
      handler { (groupId: String, artifactId: String, version: String, _: Request) =>
        if groupId == "org.webjars" && artifactId == "jquery" && version == "3.6.4" then
          Response.text("7")
        else
          Response.status(Status.NotFound)
      },
  )

  private def withStubbedService[A](run: WebJarsFileService => ZIO[Scope, Throwable, A]): ZIO[Client & TestServer, Throwable, A] =
    ZIO.scoped:
      for
        testServer <- ZIO.service[TestServer]
        _          <- testServer.install(stubRoutes)
        port       <- testServer.port
        client     <- ZIO.service[Client]
        service     = WebJarsFileServiceLive(client, TestInfrastructure.testConfig.copy(fileServiceUrl = s"http://127.0.0.1:$port"))
        result     <- run(service)
      yield result

  def spec = suite("WebJarsFileService")(
    test("getFileList works") {
      withStubbedService: webJarsFileService =>
        for
          fileList <- webJarsFileService.getFileList(gav)
        yield assertTrue(fileList.contains("META-INF/resources/webjars/jquery/3.6.4/jquery.js"))
    } @@ TestAspect.withLiveClock,
    test("getNumFiles works") {
      withStubbedService: webJarsFileService =>
        for
          numFiles <- webJarsFileService.getNumFiles(gav)
        yield assertTrue(numFiles == 7)
    } @@ TestAspect.withLiveClock,
  ).provide(Client.default, TestServer.default) @@ TestAspect.timeout(30.seconds)
