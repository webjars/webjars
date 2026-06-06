package webjars

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_http_guard.CrawlerLimiter
import com.jamesward.zio_mavencentral.MavenCentral
import org.testcontainers.containers.wait.strategy.Wait
import webjars.config.AppConfig
import webjars.routes.{AppRoutes, StaticAssets}
import webjars.utils.*
import zio.*
import zio.http.*
import zio.redis.{CodecSupplier, Redis, RedisConfig}
import zio.schema.Schema
import zio.schema.codec.{BinaryCodec, ProtobufCodec}

/**
 * Test entry-point for `reStartTest` (sbt-revolver) and `runTest`.
 *
 * Boots the full webjars server with:
 *
 *   • A `valkey/valkey:8.1` Docker container started via Testcontainers,
 *     replacing the production `REDIS_URL` config.
 *   • The cache is hydrated once at startup by GETting `/all` from
 *     `https://www.webjars.org` (always fresh, no commit-the-cache-blob in
 *     git, no Heroku credentials needed). If the fetch fails, the app
 *     still boots with an empty cache (the home page shows the "Indexing
 *     artifacts" placeholder).
 *   • The test app **never** runs the live Maven Central refresh loop —
 *     that fan-out triggers 429s against our prod IP.
 *   • A [[TestInfrastructure.MockMavenCentralDeployer]] in place of the
 *     real OSS-deploying layer, so no credentials are required and no
 *     artifacts are published to Maven Central.
 *
 * Run once: `./sbt runTest`
 * Watch + reload: `./sbt reStartTest`
 *
 * The integration test script (`./test-integration.sh`) can be pointed at
 * the resulting server (`http://localhost:9000` by default).
 */
object WebJarsTestApp extends ZIOAppDefault:

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Logging.bootstrap

  // Eagerly start a fresh valkey container so the Redis layer can be built
  // synchronously when constructed.
  private val container: GenericContainer =
    val c = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort(),
    )
    c.start()
    c

  private object ProtobufCodecSupplier extends CodecSupplier:
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec

  private val testRedisLayer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++
      ZLayer.succeed[CodecSupplier](ProtobufCodecSupplier) >>>
      Redis.singleNode.orDie

  private val testConfigLayer: ZLayer[Any, Nothing, AppConfig] =
    ZLayer.succeed(TestInfrastructure.testConfig)

  def run =
    ZIO.scoped:
      // MavenCentralWebJars is still wired into the layer (AppRoutes /deploy
      // uses it), but we intentionally NEVER call startRefreshLoop() here —
      // the test app must not hammer Maven Central. The cache is hydrated
      // once from the live /all endpoint instead; if that fails the app
      // boots with an empty cache and a warning.
      for
        appRoutes           <- ZIO.service[AppRoutes[Any]]
        searchIndex         <- ZIO.service[SearchIndex]
        popularRanking      <- ZIO.service[PopularRanking]
        _                   <- CacheHydrate.fromUrl(CacheHydrate.DefaultUrl)
                                 .tapErrorCause(c => ZIO.logWarningCause(s"cache hydration from ${CacheHydrate.DefaultUrl} failed — booting with empty cache", c))
                                 .ignore
        _                   <- searchIndex.rebuild.forkDaemon
        _                   <- popularRanking.populate.forkDaemon
        // Mirrors Main.scala — log method/url/status/duration_ms plus the
        // User-Agent so reStartTest / runTest output matches production,
        // and apply the same crawler limiter on /listfiles.
        allRoutes            = (appRoutes.routes ++ StaticAssets.routes ++ TestStaticAssets.routes) @@
                                 CrawlerLimits.middleware @@
                                 Middleware.requestLogging(loggedRequestHeaders = Set(Header.UserAgent))
        port                 = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(9000)
        _                   <- ZIO.logInfo(s"Starting test server on port $port")
        _                   <- Server.serve(allRoutes).provideSome[Client & zio.redis.Redis & MavenCentral.MavenCentralRepo & CrawlerLimiter[MavenCentral.GroupArtifactVersion]](
                                 Server.defaultWith(_.binding(java.net.InetSocketAddress("0.0.0.0", port))),
                               )
      yield ()
    .provide(
      testConfigLayer,
      Client.default,
      Cache.live,
      testRedisLayer,
      Git.live,
      GitHub.live,
      SemVer.live,
      Maven.live,
      SourceLocator.live,
      WebJarsFileService.live,
      NPM.live,
      Classic.live,
      AllDeployables.live,
      TestInfrastructure.mockMavenCentralDeployerLayer,
      MavenCentral.MavenCentralRepo.live,
      MavenCentralWebJars.live,
      DeployWebJar.live[Any],
      DeployFailureTracker.live,
      DeployJobs.live[Any],
      WebJars.live,
      PopularMetrics.live,
      PopularRanking.live,
      SearchIndex.live,
      AppRoutes.live[Any],
      CrawlerLimits.layer,
    )
