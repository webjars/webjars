package webjars

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_mavencentral.MavenCentral
import org.testcontainers.containers.wait.strategy.Wait
import webjars.utils.{PopularMetrics, PopularMetricsLive, Valkey, ValkeyLive}
import zio.*
import zio.redis.{CodecSupplier, Redis, RedisConfig}
import zio.test.*

import java.time.ZoneOffset

object PopularMetricsSpec extends ZIOSpecDefault:

  private val container: GenericContainer =
    val c = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort()
    )
    c.start()
    c

  private val redisLayer: ZLayer[Any, Nothing, Redis] =
    val valkey = ValkeyLive()
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ valkey.codecLayer >>> Redis.singleNode.orDie

  // Stand-in Valkey for PopularMetricsLive — provides the testcontainer-backed
  // Redis layer instead of the prod REDIS_URL one. close() is a no-op because
  // the testcontainer is shared across the whole suite.
  private val testValkey: Valkey = new Valkey:
    val layer: ZLayer[Any, Nothing, Redis] = redisLayer
    val codecLayer: ZLayer[Any, Nothing, CodecSupplier] = ValkeyLive().codecLayer
    def close(): Unit = ()

  private val popularMetrics: PopularMetrics = PopularMetricsLive(testValkey)

  private val groupId   = MavenCentral.GroupId("org.webjars")
  private val npmGroup  = MavenCentral.GroupId("org.webjars.npm")
  private val jqueryId  = MavenCentral.ArtifactId("jquery")
  private val reactId   = MavenCentral.ArtifactId("react")
  private val githubId  = MavenCentral.ArtifactId("github-com-Sortable-Sortable")

  // Recompute the UTC date key the same way the implementation does, so we're
  // testing the format rather than hardcoding a moving target.
  private val todayKey: UIO[String] =
    Clock.currentDateTime.map: now =>
      s"popular:${now.atZoneSameInstant(ZoneOffset.UTC).toLocalDate}"

  // recordSearchAppearance / recordListFilesClick fork-daemon their work, so
  // the call returns before Redis has been touched. Poll zScore until it
  // matches what we expect, with a hard upper bound.
  private def awaitScore(key: String, member: String, expected: Double): ZIO[Redis, Nothing, Option[Double]] =
    ZIO.serviceWithZIO[Redis](_.zScore(key, member))
      .orDie
      .repeatUntil(_.contains(expected))
      .timeout(3.seconds)
      .map(_.flatten)

  private def ttlOf(key: String): ZIO[Redis, Nothing, Duration] =
    ZIO.serviceWithZIO[Redis](_.ttl(key)).orDie

  private def keyExists(key: String): ZIO[Redis, Nothing, Boolean] =
    ZIO.serviceWithZIO[Redis](_.exists(key).map(_ > 0L)).orDie

  // Each test runs against a clean slate of the daily key it touches; tests
  // share the same Redis instance, so without this the scores accumulate
  // across cases.
  private def flushToday: ZIO[Redis, Nothing, Unit] =
    todayKey.flatMap(k => ZIO.serviceWithZIO[Redis](_.del(k)).orDie.unit)

  def spec = suite("PopularMetrics")(

    test("recordListFilesClick gives a non-github artifact the full listfiles weight"):
      for
        _      <- flushToday
        _      <- popularMetrics.recordListFilesClick(groupId, jqueryId)
        key    <- todayKey
        score  <- awaitScore(key, "org.webjars:jquery", 50.0)
      yield assertTrue(score.contains(50.0)),

    test("recordListFilesClick discounts github-com artifacts to 0.3×"):
      for
        _      <- flushToday
        _      <- popularMetrics.recordListFilesClick(npmGroup, githubId)
        key    <- todayKey
        score  <- awaitScore(key, s"org.webjars.npm:${githubId.toString}", 15.0)
      yield assertTrue(score.contains(15.0)),

    test("recordSearchAppearance writes one score per item with the right weights"):
      for
        _        <- flushToday
        _        <- popularMetrics.recordSearchAppearance(Seq(
                      (groupId, jqueryId),
                      (groupId, reactId),
                      (npmGroup, githubId),
                    ))
        key      <- todayKey
        jq       <- awaitScore(key, "org.webjars:jquery", 10.0)
        rc       <- awaitScore(key, "org.webjars:react", 10.0)
        gh       <- awaitScore(key, s"org.webjars.npm:${githubId.toString}", 3.0)
      yield assertTrue(
        jq.contains(10.0),
        rc.contains(10.0),
        gh.contains(3.0),
      ),

    test("repeated recordings accumulate (listfiles + search on the same artifact)"):
      for
        _      <- flushToday
        _      <- popularMetrics.recordSearchAppearance(Seq((groupId, jqueryId)))
        _      <- popularMetrics.recordListFilesClick(groupId, jqueryId)
        key    <- todayKey
        score  <- awaitScore(key, "org.webjars:jquery", 60.0)   // 10 + 50
      yield assertTrue(score.contains(60.0)),

    test("recordSearchAppearance with an empty list writes nothing"):
      for
        _       <- flushToday
        _       <- popularMetrics.recordSearchAppearance(Seq.empty)
        key     <- todayKey
        _       <- Live.live(ZIO.sleep(200.millis))    // give a daemon a chance to misbehave
        exists  <- keyExists(key)
      yield assertTrue(!exists),

    test("daily key carries a TTL within the configured 32-day window"):
      for
        _    <- flushToday
        _    <- popularMetrics.recordListFilesClick(groupId, jqueryId)
        key  <- todayKey
        _    <- awaitScore(key, "org.webjars:jquery", 50.0)
        ttl  <- ttlOf(key)
      yield assertTrue(
        ttl.toDays >= 28L,   // 32 minus a generous boundary for slow CI / day rollover
        ttl.toDays <= 32L,
      ),

  ).provideLayer(redisLayer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
