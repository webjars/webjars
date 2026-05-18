package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.routes.{AppRoutes, StaticAssets}
import webjars.utils.*
import zio.*
import zio.direct.*
import zio.http.*
import zio.redis.Redis

object Main extends ZIOAppDefault:

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.removeDefaultLoggers >>> zio.logging.consoleLogger()

  def run =
    ZIO.scoped:
      for
        appRoutes <- ZIO.service[AppRoutes[MavenCentral.Deploy.Sonatype]]
        mavenCentralWebJars <- ZIO.service[MavenCentralWebJars]
        searchIndex <- ZIO.service[SearchIndex]
        popularRanking <- ZIO.service[PopularRanking]
        _ <- searchIndex.rebuild.forkDaemon
        _ <- popularRanking.populate.forkDaemon
        _ <- mavenCentralWebJars.startRefreshLoop()
        allRoutes = appRoutes.routes ++ StaticAssets.routes
        port = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(9000)
        _ <- ZIO.logInfo(s"Starting server on port $port")
        _ <- Server.serve(allRoutes).provideSome[Client & Redis & MavenCentral.Deploy.Sonatype](
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
      MavenCentral.Deploy.Sonatype.Live,
      MavenCentralWebJars.live,
      DeployWebJar.live[MavenCentral.Deploy.Sonatype],
      DeployJobs.live[MavenCentral.Deploy.Sonatype],
      WebJars.live,
      PopularMetrics.live,
      PopularRanking.live,
      SearchIndex.live,
      AppRoutes.live[MavenCentral.Deploy.Sonatype],
    )
