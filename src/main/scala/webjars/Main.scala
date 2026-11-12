package webjars

import com.jamesward.zio_http_guard.CrawlerLimiter
import com.jamesward.zio_http_guard.guard.{ClientId, Guard}
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
    Logging.bootstrap

  def run =
    ZIO.scoped:
      for
        appRoutes <- ZIO.service[AppRoutes[MavenCentral.Deploy.Sonatype]]
        mavenCentralWebJars <- ZIO.service[MavenCentralWebJars]
        searchIndex <- ZIO.service[SearchIndex]
        popularRanking <- ZIO.service[PopularRanking]
        guard <- ZIO.service[Guard]
        _ <- searchIndex.rebuild.forkDaemon
        _ <- popularRanking.populate.forkDaemon
        _ <- mavenCentralWebJars.startRefreshLoop()
        // Request lines carry the headers that tell flood traffic apart from
        // real visitors. The guard (see `WebJarsGuard`) sits outside the
        // logger so each line carries client_ip / guard_tier / guard_reason;
        // the crawler limiter sits inside so its 429s are still logged.
        allRoutes = (appRoutes.routes ++ StaticAssets.routes) @@
          CrawlerLimits.middleware @@
          Middleware.requestLogging(loggedRequestHeaders = ClientId.identifyingHeaders) @@
          guard.middleware
        port = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(9000)
        _ <- ZIO.logInfo(s"Starting server on port $port")
        _ <- Server.serve(allRoutes).provideSome[Client & Redis & MavenCentral.Deploy.Sonatype & MavenCentral.MavenCentralRepo & CrawlerLimiter[MavenCentral.GroupArtifactVersion]](
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
      SourceLocator.live,
      WebJarsFileService.live,
      FileListCache.live,
      WebJarsGuard.layer,
      NPM.live,
      Classic.live,
      AllDeployables.live,
      MavenCentralDeployer.live,
      MavenCentral.Deploy.Sonatype.Live,
      MavenCentral.MavenCentralRepo.live,
      MavenCentralWebJars.live,
      DeployWebJar.live[MavenCentral.Deploy.Sonatype],
      DeployFailureTracker.live,
      DeployJobs.live[MavenCentral.Deploy.Sonatype],
      WebJars.live,
      PopularMetrics.live,
      PopularRanking.live,
      SearchIndex.live,
      AppRoutes.live[MavenCentral.Deploy.Sonatype],
      CrawlerLimits.layer,
    )
