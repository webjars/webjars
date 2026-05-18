package webjars

import com.dimafeng.testcontainers.GenericContainer
import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.utils.*
import zio.*
import zio.http.*
import zio.redis.{Redis, RedisConfig}

import org.testcontainers.containers.wait.strategy.Wait

object TestInfrastructure:

  val testConfig: AppConfig = AppConfig(
    githubAuthToken = sys.env.get("GITHUB_TOKEN"),
    ossGpgKey = None,
    ossGpgPass = None,
    ossDeployUsername = None,
    ossDeployPassword = None,
    fileServiceUrl = "https://webjars-file-service.herokuapp.com",
    mavenCentralLimit = None,
    mavenCentralRefreshInterval = None,
    useWebJarsCdn = false,
  )

  class MockMavenCentralDeployer extends MavenCentralDeployer:
    def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Any, Throwable, Unit] =
      ZIO.logInfo(s"[mock] would deploy $gav (jar ${jar.length} bytes, pom ${pom.length} chars)")

  val mockMavenCentralDeployerLayer: ULayer[MavenCentralDeployer] =
    ZLayer.succeed(MockMavenCentralDeployer())

  // No-op SearchIndex for tests that don't exercise search. Snapshot is
  // always empty; rebuild is a no-op so `refreshAll` doesn't try to assemble
  // the index against test data it doesn't care about.
  val noopSearchIndex: SearchIndex = new SearchIndex:
    def snapshot: UIO[List[webjars.models.WebJar]] = ZIO.succeed(List.empty)
    def rebuild: UIO[Unit] = ZIO.unit

  def services(client: Client): Services =
    val cache = CacheLive()
    val valkey = ValkeyLive()
    val git = GitLive(client)
    val gitHub = GitHubLive(client, testConfig, cache)
    val semVer = SemVerLive(client)
    val maven = MavenLive(git, semVer)
    val licenseDetector = LicenseDetectorLive(client, testConfig.githubAuthToken)
    val sourceLocator = SourceLocatorLive(client, git)
    val webJarsFileService = WebJarsFileServiceLive(client, testConfig)
    val npm = NPMLive(client, licenseDetector, git, gitHub, maven, semVer)
    val classic = ClassicLive(client, licenseDetector, gitHub, cache, testConfig, npm)
    val allDeployables = AllDeployablesLive(classic, npm)
    val mavenCentralDeployer = MavenCentralDeployerLive(testConfig)
    val mavenCentralWebJars = MavenCentralWebJarsLive(testConfig, webJarsFileService, valkey, allDeployables, noopSearchIndex)
    val deployWebJar = DeployWebJarLive(mavenCentralWebJars, mavenCentralDeployer, sourceLocator)
    Services(
      config = testConfig,
      cache = cache,
      valkey = valkey,
      git = git,
      gitHub = gitHub,
      semVer = semVer,
      maven = maven,
      licenseDetector = licenseDetector,
      sourceLocator = sourceLocator,
      webJarsFileService = webJarsFileService,
      npm = npm,
      classic = classic,
      allDeployables = allDeployables,
      mavenCentralDeployer = mavenCentralDeployer,
      mavenCentralWebJars = mavenCentralWebJars,
      deployWebJar = deployWebJar,
    )

  case class Services(
    config: AppConfig,
    cache: Cache,
    valkey: Valkey,
    git: Git,
    gitHub: GitHub,
    semVer: SemVer,
    maven: Maven,
    licenseDetector: LicenseDetector,
    sourceLocator: SourceLocator,
    webJarsFileService: WebJarsFileService,
    npm: NPM,
    classic: Classic,
    allDeployables: AllDeployables,
    mavenCentralDeployer: MavenCentralDeployer,
    mavenCentralWebJars: MavenCentralWebJars,
    deployWebJar: DeployWebJar,
  )

  val servicesLayer: ZLayer[Client, Nothing, Services] =
    ZLayer.fromFunction(services)

  def valkeyContainer(): GenericContainer =
    val container = GenericContainer(
      dockerImage = "valkey/valkey:8.1",
      exposedPorts = Seq(6379),
      waitStrategy = Wait.forListeningPort()
    )
    container.start()
    container

  def valkeyLayer(container: GenericContainer): ZLayer[Any, Nothing, Redis] =
    val valkey = ValkeyLive()
    ZLayer.succeed(RedisConfig(container.host, container.mappedPort(6379))) ++ valkey.codecLayer >>> Redis.singleNode.orDie
