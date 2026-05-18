package webjars

import com.dimafeng.testcontainers.GenericContainer
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
 *   • [[TestInfrastructure.testConfig]] with `mavenCentralLimit = Some(5)`
 *     to keep the artifact-refresh loop fast for local development.
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
    Runtime.removeDefaultLoggers >>> zio.logging.consoleLogger()

  /** Eagerly start a fresh valkey container so the `Valkey` service can
   *  return a layer pointing at the right host:port synchronously. The Redis
   *  env is materialized once at startup and cached; the returned layer is a
   *  `succeedEnvironment` wrapper so per-call `.provide(valkey.layer)` reuses
   *  the same connection (matches the prod `ValkeyLive` shape). Without this
   *  cache, every call site opens a fresh Redis connection. */
  private def testValkey(): Valkey =
    val container = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort(),
    )
    container.start()

    new Valkey:
      object ProtobufCodecSupplier extends CodecSupplier:
        def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec

      val codecLayer: ZLayer[Any, Nothing, CodecSupplier] =
        ZLayer.succeed(ProtobufCodecSupplier)

      private val runtime = Runtime.default

      private val scopeCloseable: Scope.Closeable =
        Unsafe.unsafe { implicit u => runtime.unsafe.run(Scope.make).getOrThrow() }

      private lazy val redisEnv: ZEnvironment[Redis] =
        Unsafe.unsafe { implicit u =>
          runtime.unsafe.run(
            (ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ codecLayer >>> Redis.singleNode.orDie)
              .build
              .provideEnvironment(ZEnvironment(scopeCloseable))
          ).getOrThrow()
        }

      val layer: ZLayer[Any, Nothing, Redis] = ZLayer.succeedEnvironment(redisEnv)

      def close(): Unit =
        Unsafe.unsafe { implicit u =>
          runtime.unsafe.run(scopeCloseable.close(Exit.unit)).getOrThrow()
        }
        container.stop()

  private val testValkeyLayer: ZLayer[Any, Nothing, Valkey] =
    ZLayer.succeed(testValkey())

  private val devConfig: AppConfig =
    TestInfrastructure.testConfig.copy(mavenCentralLimit = Some(5))

  private val testConfigLayer: ZLayer[Any, Nothing, AppConfig] =
    ZLayer.succeed(devConfig)

  def run =
    ZIO.scoped:
      for
        appRoutes           <- ZIO.service[AppRoutes]
        mavenCentralWebJars <- ZIO.service[MavenCentralWebJars]
        searchIndex         <- ZIO.service[SearchIndex]
        _                   <- searchIndex.rebuild.forkDaemon
        _                   <- mavenCentralWebJars.startRefreshLoop()
        allRoutes            = appRoutes.routes ++ StaticAssets.routes ++ TestStaticAssets.routes
        port                 = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(9000)
        _                   <- ZIO.logInfo(s"Starting test server on port $port")
        _                   <- Server.serve(allRoutes).provide(
                                 Server.defaultWith(_.binding(java.net.InetSocketAddress("0.0.0.0", port))),
                               )
      yield ()
    .provide(
      testConfigLayer,
      Client.default,
      Cache.live,
      testValkeyLayer,
      Git.live,
      GitHub.live,
      SemVer.live,
      Maven.live,
      LicenseDetector.live,
      SourceLocator.live,
      WebJarsFileService.live,
      NPM.live,
      Classic.live,
      AllDeployables.live,
      TestInfrastructure.mockMavenCentralDeployerLayer,
      MavenCentralWebJars.live,
      DeployWebJar.live,
      DeployJobs.live,
      WebJars.live,
      PopularMetrics.live,
      SearchIndex.live,
      AppRoutes.live,
    )
