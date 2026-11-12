package webjars

import com.jamesward.zio_http_guard.CrawlerLimiter
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
        loadShedding <- ZIO.service[LoadShedding]
        badActors <- ZIO.service[BadActors]
        trafficStats <- ZIO.service[TrafficStats]
        _ <- searchIndex.rebuild.forkDaemon
        _ <- popularRanking.populate.forkDaemon
        _ <- mavenCentralWebJars.startRefreshLoop()
        // Log every request with method/url/status/duration_ms plus the
        // headers that tell flood traffic apart from real visitors: the
        // client IP (last X-Forwarded-For hop on Heroku), the User-Agent,
        // and the headers a real browser sends but a script copying its
        // User-Agent often doesn't (Referer, Sec-Fetch-*, Sec-CH-UA client
        // hints, Accept-*). Both limiters sit *inside* the logging aspect so
        // their 429 / 503 responses still get logged; `badActors.screen`
        // sits outside it, blocking identified flood requests and adding
        // `traffic_class` to each line.
        allRoutes = (appRoutes.routes ++ StaticAssets.routes) @@
          CrawlerLimits.middleware @@
          loadShedding.limit @@
          Middleware.requestLogging(loggedRequestHeaders = Set(
            Header.UserAgent,
            Header.Referer,
            Header.Accept,
            Header.AcceptLanguage,
            Header.AcceptEncoding,
            Header.SchemaHeaderType[String]("X-Forwarded-For"),
            Header.SchemaHeaderType[String]("Sec-Fetch-Site"),
            Header.SchemaHeaderType[String]("Sec-Fetch-Mode"),
            Header.SchemaHeaderType[String]("Sec-Fetch-Dest"),
            Header.SchemaHeaderType[String]("Sec-CH-UA"),
            Header.SchemaHeaderType[String]("Sec-CH-UA-Platform"),
          )) @@
          badActors.screen @@
          trafficStats.middleware
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
      LoadShedding.layer,
      BadActors.layer,
      TrafficStats.layer,
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
