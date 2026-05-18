package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import webjars.utils.*
import zio.*
import zio.http.{Client, ZClientAspect}
import zio.redis.{CodecSupplier, Redis, RedisConfig}
import zio.schema.Schema
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.test.*

// End-to-end smoke test of the missing-only + tombstones refresh:
//   1. Hydrate the cache fully from the live /all endpoint.
//   2. First refreshAll: handful of "missing" artifacts (those that
//      appeared on MC after our /all snapshot, plus orphan listings).
//      Successes go in the cache; failures (404 on metadata, etc.) go
//      in the per-group tombstone set.
//   3. Second refreshAll: with the cache and tombstones populated, this
//      run should do essentially no work — just the two per-group
//      `fetchArtifactIds` calls.
//
// Asserts:
//   - First run: bounded MC requests (~2 per group + ~missing count).
//   - Second run: same `fetchArtifactIds`, but the missing set should now
//     be empty (everything either landed in cache or got tombstoned), so
//     no `refreshArtifact` calls fire. Tight 5s time budget makes the
//     "fast second run" claim measurable.
//
// Regression guard: if anyone reintroduces a per-cached-artifact
// fan-out, OR removes tombstones so failures get retried every cycle,
// this test will time out.
//
// Gated behind `RUN_REFRESH_INTEGRATION=1` — even at single-digit MC
// requests per run it's still real network traffic.
object RefresherIntegrationSpec extends ZIOSpecDefault:

  private val container: GenericContainer =
    val c = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort()
    )
    c.start()
    c

  private val codecSupplier: CodecSupplier = new CodecSupplier:
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec

  private val redisLayer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++
      ZLayer.succeed(codecSupplier) >>> Redis.singleNode.orDie


  private val classicGroup = MavenCentral.GroupId("org.webjars")
  private val npmGroup     = MavenCentral.GroupId("org.webjars.npm")
  private val groupIds     = Set(classicGroup, npmGroup)

  // First-run budget covers the per-group `fetchArtifactIds` plus the
  // per-missing-artifact `refreshArtifact` calls. With our /all snapshot
  // mirroring MC, "missing" is small (orphan listings + brand-new
  // publishes), so 30s is plenty even with 429-retry backoff.
  private val FirstRunBudget:  Duration = 30.seconds
  // Second-run budget is much tighter — after the first run, missing
  // = ∅ and the only MC calls are the per-group fetchArtifactIds (2
  // GETs per group, ~4 total).
  private val SecondRunBudget: Duration =  5.seconds

  private def runRefresh(mc: MavenCentralWebJars): ZIO[Client & Redis, Throwable, Long] =
    for
      start  <- Clock.nanoTime
      _      <- mc.refreshAll(groupIds)
      end    <- Clock.nanoTime
    yield (end - start) / 1_000_000L

  def spec = suite("RefresherIntegration")(

    test("refresh #1 fetches the diff, refresh #2 is a no-op (tombstones in effect)"):
      ZIO.serviceWithZIO[Client]: client =>
        val mcWebjars = MavenCentralWebJarsLive(
          TestInfrastructure.testConfig,
          WebJarsFileServiceLive(client, TestInfrastructure.testConfig),
          new AllDeployables:
            def fromGroupId(g: MavenCentral.GroupId)  = None
            def fromName(n: String)                   = None
            def groupIds(): Set[MavenCentral.GroupId] = RefresherIntegrationSpec.groupIds
          ,
          TestInfrastructure.noopSearchIndex,
        )
        for
          hydrated     <- CacheHydrate.fromUrl(CacheHydrate.DefaultUrl)
          _            <- ZIO.logInfo(s"--- hydrated $hydrated artifacts; refresh #1 ---")
          firstMs      <- runRefresh(mcWebjars)
          tombs1       <- ZIO.foreach(groupIds.toList)(g => WebJarsCache.getTombstones(g).map(_.size)).map(_.sum)
          _            <- ZIO.logInfo(s"--- refresh #1 took ${firstMs}ms, $tombs1 tombstones written ---")
          _            <- ZIO.logInfo("--- refresh #2 (should be a no-op) ---")
          secondMs     <- runRefresh(mcWebjars)
          tombs2       <- ZIO.foreach(groupIds.toList)(g => WebJarsCache.getTombstones(g).map(_.size)).map(_.sum)
          _            <- ZIO.logInfo(s"--- refresh #2 took ${secondMs}ms, tombstones=$tombs2 ---")
        yield assertTrue(
          hydrated > 1000,
          // First run is loose: a few "missing" calls + retries are fine,
          // as long as the per-cached-artifact storm doesn't return.
          firstMs < FirstRunBudget.toMillis,
          // Second run is tight: tombstones should mean zero refreshArtifact
          // calls, leaving only the per-group fetchArtifactIds.
          secondMs < SecondRunBudget.toMillis,
          // Tombstone count is stable across runs — once written, no new
          // failures should be discovered on the second pass.
          tombs2 == tombs1,
        )
    @@ TestAspect.withLiveClock @@ TestAspect.timeout(2.minutes)
      ,
  ).provide(
    Client.default.update(_ @@ ZClientAspect.requestLogging()),
    redisLayer,
  ) @@ TestAspect.ifEnvSet("RUN_REFRESH_INTEGRATION")
