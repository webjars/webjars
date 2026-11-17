package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.utils.*
import zio.*
import zio.http.*

import javax.sql.DataSource

object TestInfrastructure:

  val testConfig: AppConfig = AppConfig(
    githubAuthToken = sys.env.get("GITHUB_TOKEN"),
    ossGpgKey = None,
    ossGpgPass = None,
    ossDeployUsername = None,
    ossDeployPassword = None,
    fileServiceUrl = "https://webjars-file-service.herokuapp.com",
    mavenCentralRefreshInterval = None,
    useWebJarsCdn = false,
    // Default to None in tests so the live tracker is opt-in: only
    // tests that wire `DeployFailureTracker` explicitly will exercise
    // GitHub-side behavior. Production overrides this to `webjars/webjars`.
    deployFailureIssuesRepo = None,
  )

  /** A `DeployFailureTracker` that always short-circuits — useful for
   *  tests that don't care about issue filing. */
  object NoopDeployFailureTracker extends DeployFailureTracker:
    def trackFailure(deployKey: DeployFailureTracker.DeployKey, failure: DeployFailure, deployLog: zio.Chunk[String]): zio.URIO[DataSource, Option[zio.http.URL]] =
      ZIO.none
    def resolveSuccess(deployKey: DeployFailureTracker.DeployKey): zio.URIO[DataSource, Unit] =
      ZIO.unit

  val noopDeployFailureTrackerLayer: ZLayer[Any, Nothing, DeployFailureTracker] =
    ZLayer.succeed[DeployFailureTracker](NoopDeployFailureTracker)

  /** Mock deployer with `Env = Any` — no Sonatype layer needed.
   *  `ascSign` opportunistically signs if `OSS_GPG_KEY` is set in the env
   *  (so manual end-to-end tests can verify a real signature), otherwise
   *  returns `None`. */
  class MockMavenCentralDeployer extends MavenCentralDeployer[Any]:
    def publish(gav: MavenCentral.GroupArtifactVersion, jar: Deployable.ArchiveStream, pom: String): ZIO[Any, Throwable, Unit] =
      jar.runCount.flatMap(size => ZIO.logInfo(s"[mock] would deploy $gav (jar $size bytes, pom ${pom.length} chars)"))

    override def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]] =
      ZIO.systemWith(_.env("OSS_GPG_KEY")).flatMap:
        case Some(gpgKey) =>
          ZIO.systemWith(_.env("OSS_GPG_PASS")).flatMap: maybePass =>
            ZIO.scoped:
              MavenCentral.Signer.make(gpgKey, maybePass).build
                .flatMap(env => env.get[MavenCentral.Signer].ascSign(toSign))
        case None =>
          ZIO.none

  val mockMavenCentralDeployerLayer: ULayer[MavenCentralDeployer[Any]] =
    ZLayer.succeed(MockMavenCentralDeployer())

  // No-op SearchIndex for tests that don't exercise search. Snapshot is
  // always empty; rebuild is a no-op so `refreshAll` doesn't try to assemble
  // the index against test data it doesn't care about.
  val noopSearchIndex: SearchIndex = new SearchIndex:
    def snapshot: UIO[List[webjars.models.WebJar]] = ZIO.succeed(List.empty)
    def rebuild: URIO[DataSource, Unit] = ZIO.unit

  // No-op PopularRanking for tests that don't exercise the home page.
  // Snapshot is always empty; populate and rebuild are no-ops so
  // `refreshAll` doesn't try to compute aggregate scores against test
  // data it doesn't care about.
  val noopPopularRanking: PopularRanking = new PopularRanking:
    def snapshot: UIO[List[webjars.models.WebJar]] = ZIO.succeed(List.empty)
    def populate: URIO[DataSource, Unit]                = ZIO.unit
    def rebuild:  URIO[DataSource, Unit]                = ZIO.unit

  // A freshly migrated, empty Postgres database (see `PostgresContainer`),
  // for specs that only need the `DataSource` requirement satisfied.
  val databaseLayer: ZLayer[Any, Nothing, DataSource] =
    PostgresContainer.freshDatabase.orDie
