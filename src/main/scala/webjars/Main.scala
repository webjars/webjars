package webjars

import webjars.config.AppConfig
import webjars.routes.{AppRoutes, StaticAssets}
import webjars.utils.*
import zio.*
import zio.direct.*
import zio.http.*

object Main extends ZIOAppDefault:

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.removeDefaultLoggers >>> zio.logging.consoleLogger()

  def run =
    ZIO.scoped:
      for
        appRoutes <- ZIO.service[AppRoutes]
        mavenCentralWebJars <- ZIO.service[MavenCentralWebJars]
        searchIndex <- ZIO.service[SearchIndex]
        _ <- searchIndex.rebuild.forkDaemon
        _ <- mavenCentralWebJars.startRefreshLoop()
        allRoutes = appRoutes.routes ++ StaticAssets.routes
        port = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(9000)
        _ <- ZIO.logInfo(s"Starting server on port $port")
        _ <- Server.serve(allRoutes).provide(
          Server.defaultWith(_.binding(java.net.InetSocketAddress("0.0.0.0", port))),
        )
      yield ()
    .provide(
      AppConfig.live,
      Client.default,
      Cache.live,
      Valkey.live,
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
      MavenCentralDeployer.live,
      MavenCentralWebJars.live,
      DeployWebJar.live,
      DeployJobs.live,
      WebJars.live,
      PopularMetrics.live,
      SearchIndex.live,
      AppRoutes.live,
    )
