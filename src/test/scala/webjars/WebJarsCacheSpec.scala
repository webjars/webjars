package webjars

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_mavencentral.MavenCentral
import webjars.models.WebJarVersion
import webjars.utils.{Valkey, WebJarsCache}
import webjars.utils.WebJarsCache.WebJarMeta
import zio.*
import zio.redis.{Redis, RedisConfig}
import zio.test.*

import java.time.ZonedDateTime
import org.testcontainers.containers.wait.strategy.Wait

object WebJarsCacheSpec extends ZIOSpecDefault:

  private val container: GenericContainer =
    val c = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort()
    )
    c.start()
    c

  private val valkeyLayer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ Valkey.codecSupplierLayer >>> Redis.singleNode.orDie

  def spec = suite("WebJarsCache")(
    test("store and retrieve artifact details") {
      val groupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      for
        _ <- WebJarsCache.setArtifactDetails(groupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", Some(1234)), WebJarVersion("3.2.0", None)), ZonedDateTime.now()))
        jquery <- WebJarsCache.getArtifact(groupArtifact)
        artifacts <- WebJarsCache.getArtifacts(groupArtifact.groupId)
      yield assertTrue(
        jquery.isDefined,
        artifacts.get(MavenCentral.ArtifactId("jquery")).isDefined,
      )
    },
    test("search") {
      val jqueryGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      val aceGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("ace"))
      for
        _ <- WebJarsCache.setArtifactDetails(jqueryGroupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", Some(1234))), ZonedDateTime.now()))
        _ <- WebJarsCache.setArtifactDetails(aceGroupArtifact, WebJarMeta("ACE", "https://ace.com", List(WebJarVersion("1.0.0", Some(523))), ZonedDateTime.now()))
        artifactDetails <- WebJarsCache.getArtifacts(MavenCentral.GroupId("org.webjars"), None, Some("C"))
      yield assertTrue(
        artifactDetails.size == 1,
        artifactDetails.exists(_._1.toString == "ace"),
      )
    },
    test("limit") {
      val jqueryGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      val aceGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("ace"))
      for
        _ <- WebJarsCache.setArtifactDetails(jqueryGroupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", Some(1234))), ZonedDateTime.now()))
        _ <- WebJarsCache.setArtifactDetails(aceGroupArtifact, WebJarMeta("ACE", "https://ace.com", List(WebJarVersion("1.0.0", Some(523))), ZonedDateTime.now()))
        artifactDetails <- WebJarsCache.getArtifacts(MavenCentral.GroupId("org.webjars"), Some(1))
      yield assertTrue(artifactDetails.size == 1)
    },
    test("update version") {
      val jqueryGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      for
        _ <- WebJarsCache.setArtifactDetails(jqueryGroupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", None)), ZonedDateTime.now()))
        _ <- WebJarsCache.updateVersion(jqueryGroupArtifact, "3.2.1", 123)
        artifactDetails <- WebJarsCache.getArtifact(jqueryGroupArtifact)
      yield assertTrue(artifactDetails.head.versions.head.numFiles.contains(123))
    },
  ).provideLayer(valkeyLayer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
