package webjars

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_mavencentral.MavenCentral
import org.testcontainers.containers.wait.strategy.Wait
import webjars.utils.{Valkey, WebJarsCache}
import zio.*
import zio.redis.{Redis, RedisConfig}
import zio.test.*

object PendingDeploysSpec extends ZIOSpecDefault:

  private val container: GenericContainer =
    val c = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort()
    )
    c.start()
    c

  private val redisLayer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ Valkey.codecSupplierLayer >>> Redis.singleNode.orDie

  private val flushAll: ZIO[Redis, Nothing, Unit] =
    ZIO.serviceWithZIO[Redis](_.flushall().orDie.unit)

  private val classicGroup = MavenCentral.GroupId("org.webjars")
  private val jqueryId     = MavenCentral.ArtifactId("jquery")

  private def pending(version: String) =
    WebJarsCache.PendingDeploy(classicGroup, jqueryId, MavenCentral.Version(version))

  def spec = suite("PendingDeploys")(

    test("addPendingDeploy then getPendingDeploys round-trips a single GAV"):
      for
        _    <- flushAll
        _    <- WebJarsCache.addPendingDeploy(pending("3.7.0"))
        all  <- WebJarsCache.getPendingDeploys
      yield assertTrue(all == Set(pending("3.7.0"))),

    test("multiple versions of the same artifact coexist"):
      for
        _    <- flushAll
        _    <- WebJarsCache.addPendingDeploy(pending("3.7.0"))
        _    <- WebJarsCache.addPendingDeploy(pending("3.7.1"))
        _    <- WebJarsCache.addPendingDeploy(pending("3.7.2"))
        all  <- WebJarsCache.getPendingDeploys
      yield assertTrue(all.map(_.version.toString) == Set("3.7.0", "3.7.1", "3.7.2")),

    test("removePendingDeploy removes just the requested GAV"):
      for
        _    <- flushAll
        _    <- WebJarsCache.addPendingDeploy(pending("3.7.0"))
        _    <- WebJarsCache.addPendingDeploy(pending("3.7.1"))
        _    <- WebJarsCache.removePendingDeploy(pending("3.7.0"))
        all  <- WebJarsCache.getPendingDeploys
      yield assertTrue(all == Set(pending("3.7.1"))),

    test("cullStalePendingDeploys removes entries older than the threshold"):
      // Add three entries at "now", then advance the TestClock 49 hours and
      // cull with a 48-hour threshold — all three should be gone.
      for
        _      <- flushAll
        _      <- WebJarsCache.addPendingDeploy(pending("3.7.0"))
        _      <- WebJarsCache.addPendingDeploy(pending("3.7.1"))
        _      <- WebJarsCache.addPendingDeploy(pending("3.7.2"))
        _      <- TestClock.adjust(49.hours)
        culled <- WebJarsCache.cullStalePendingDeploys(48.hours)
        remain <- WebJarsCache.getPendingDeploys
      yield assertTrue(culled == 3L, remain.isEmpty),

    test("cullStalePendingDeploys leaves fresh entries alone"):
      for
        _      <- flushAll
        _      <- WebJarsCache.addPendingDeploy(pending("3.7.0"))
        _      <- TestClock.adjust(1.hour)
        culled <- WebJarsCache.cullStalePendingDeploys(48.hours)
        remain <- WebJarsCache.getPendingDeploys
      yield assertTrue(culled == 0L, remain == Set(pending("3.7.0"))),

  ).provideLayer(redisLayer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
