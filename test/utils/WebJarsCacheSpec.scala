package utils

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_mavencentral.MavenCentral
import models.WebJarVersion
import org.apache.pekko.util.Timeout
import org.testcontainers.containers.wait.strategy.Wait
import play.api.inject.ApplicationLifecycle
import play.api.test.*
import utils.Adapter.*
import utils.WebJarsCache.WebJarMeta
import zio.redis.{CodecSupplier, Redis, RedisConfig}
import zio.{ZIO, ZLayer}

import java.time.ZonedDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.concurrent.duration.*

class WebJarsCacheSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = Timeout(60.seconds)

  "WebJarsCache" should {
    "store and retrieve artifact details" in {
      val groupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      val prog = for
        _ <- WebJarsCache.setArtifactDetails(groupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", Some(1234)), WebJarVersion("3.2.0", None)), ZonedDateTime.now()))
//        _ <- redisDump
        jquery <- WebJarsCache.getArtifact(groupArtifact)
        artifacts <- WebJarsCache.getArtifacts(groupArtifact.groupId)
      yield
        (jquery, artifacts)

      val (jquery, artifacts) = await(prog.runToFuture(application.injector.instanceOf[Valkey].layer))

      jquery `must` beSome
      artifacts.get(MavenCentral.ArtifactId("jquery")) `must` beSome
    }
    "search" in {
      val jqueryGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      val aceGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("ace"))

      val prog = for
        _ <- WebJarsCache.setArtifactDetails(jqueryGroupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", Some(1234))), ZonedDateTime.now()))
        _ <- WebJarsCache.setArtifactDetails(aceGroupArtifact, WebJarMeta("ACE", "https://ace.com", List(WebJarVersion("1.0.0", Some(523))), ZonedDateTime.now()))
//        _ <- redisDump
        artifactDetails <- WebJarsCache.getArtifacts(MavenCentral.GroupId("org.webjars"), None, Some("C"))
      yield
        artifactDetails

      val artifactDetails = await(prog.runToFuture(application.injector.instanceOf[Valkey].layer))
      artifactDetails.size `must` beEqualTo(1)
      artifactDetails.exists(_._1.toString == "ace") `must` beTrue
    }
    "limit" in {
      val jqueryGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))
      val aceGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("ace"))

      val prog = for
        _ <- WebJarsCache.setArtifactDetails(jqueryGroupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", Some(1234))), ZonedDateTime.now()))
        _ <- WebJarsCache.setArtifactDetails(aceGroupArtifact, WebJarMeta("ACE", "https://ace.com", List(WebJarVersion("1.0.0", Some(523))), ZonedDateTime.now()))
//        _ <- redisDump
        artifactDetails <- WebJarsCache.getArtifacts(MavenCentral.GroupId("org.webjars"), Some(1))
      yield
        artifactDetails

      val artifactDetails = await(prog.runToFuture(application.injector.instanceOf[Valkey].layer))
      artifactDetails.size `must` beEqualTo(1)
    }
    "update version" in {
      val jqueryGroupArtifact = MavenCentral.GroupArtifact(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("jquery"))

      val prog = for
        _ <- WebJarsCache.setArtifactDetails(jqueryGroupArtifact, WebJarMeta("jQuery", "https://jquery.com", List(WebJarVersion("3.2.1", None)), ZonedDateTime.now()))
        _ <- WebJarsCache.updateVersion(jqueryGroupArtifact, "3.2.1", 123)
        //        _ <- redisDump
        artifactDetails <- WebJarsCache.getArtifact(jqueryGroupArtifact)
      yield
        artifactDetails

      val artifactDetails = await(prog.runToFuture(application.injector.instanceOf[Valkey].layer))
      artifactDetails.head.versions.head.numFiles `must` beSome(123)
    }
  }
}

@Singleton
class ValkeyTest @Inject() (lifecycle: ApplicationLifecycle) extends Valkey:

  val container = GenericContainer(
    dockerImage = "valkey/valkey:8.1",
    exposedPorts = Seq(6379),
    waitStrategy = Wait.forListeningPort()
  )

  container.start()

  lifecycle.addStopHook { () =>
    Future.successful(container.stop())
  }

  val layer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ codecLayer >>> Redis.singleNode.orDie

  val redisDump: ZIO[Redis, Throwable, Unit] = ZIO.serviceWithZIO[Redis]: redis =>
    redis.keys("*").returning[String].flatMap: keys =>
      ZIO.foreach(keys): key =>
        redis.hGetAll(key).returning[String, String].debug(key)
    .unit
