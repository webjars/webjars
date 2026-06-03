package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.TestInfrastructure.testConfig
import webjars.utils.*
import zio.*
import zio.compress.*
import zio.http.Client
import zio.stream.*
import zio.test.*

import java.io.FileNotFoundException
import scala.xml.Elem

object DeploySpec extends ZIOSpecDefault:

  /** A `MavenCentralDeployer[Any]` that captures the (gav, jar, pom) of the
   *  most recent `publish` call instead of uploading to Sonatype. Lets the
   *  redeploy test inspect the produced artifacts without ever talking to
   *  Maven Central. `ascSign` returns `None` so the deployer skips signing. */
  final class CapturingDeployer(ref: Ref[Option[(MavenCentral.GroupArtifactVersion, Array[Byte], String)]])
      extends MavenCentralDeployer[Any]:
    def captured: UIO[Option[(MavenCentral.GroupArtifactVersion, Array[Byte], String)]] = ref.get
    def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Any, Throwable, Unit] =
      ref.set(Some((gav, jar, pom)))
    def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]] = ZIO.none

  /** Same shape as `DeployWebJarSpec.MockMavenCentralWebJars`: claims no POM
   *  exists for any GAV, so `webJarNotYetDeployed` lets the deploy proceed.
   *  Tests provide existing-versions independently. Named distinctly to
   *  avoid a top-level clash with the one in `DeployWebJarSpec`. */
  private class NoPomMavenCentralWebJars(config: webjars.config.AppConfig, webJarsFileService: WebJarsFileService, allDeployables: AllDeployables)
      extends MavenCentralWebJarsLive(config, webJarsFileService, allDeployables, TestInfrastructure.noopSearchIndex):
    override def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope & Client, Throwable, Elem] =
      ZIO.fail(new FileNotFoundException("no mock pom"))

  /** Build a fully-wired `(deployWebJar, allDeployables, capturingDeployer)`
   *  triple using mocks that don't touch Sonatype or persist to Redis. */
  private def withRedeploy[A](
    f: (DeployWebJar[Any], AllDeployables, CapturingDeployer) => ZIO[Scope & Client & zio.redis.Redis, Throwable, A]
  ): ZIO[Client & zio.redis.Redis, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val config             = testConfig
        val cache              = CacheLive()
        val git                = GitLive(client)
        val gitHub             = GitHubLive(client, config, cache)
        val semVer             = SemVerLive(client)
        val maven              = MavenLive(git, semVer)
        val licenseDetector    = LicenseDetectorLive(client, testConfig.githubAuthToken)
        val sourceLocator      = SourceLocatorLive(client, git)
        val webJarsFileService = WebJarsFileServiceLive(client, config)
        val npm                = NPMLive(client, licenseDetector, git, gitHub, maven, semVer)
        val classic            = ClassicLive(client, licenseDetector, gitHub, cache, config, npm)
        val allDeployables     = AllDeployablesLive(classic, npm)
        val mavenCentralWebJars: MavenCentralWebJars = NoPomMavenCentralWebJars(config, webJarsFileService, allDeployables)
        for
          capturedRef <- Ref.make[Option[(MavenCentral.GroupArtifactVersion, Array[Byte], String)]](None)
          capturing    = CapturingDeployer(capturedRef)
          deployWebJar: DeployWebJar[Any] = DeployWebJarLive[Any](mavenCentralWebJars, capturing, sourceLocator)
          result      <- f(deployWebJar, allDeployables, capturing)
        yield result
      }
    }

  private def jarEntryNames(jar: Array[Byte]): ZIO[Any, Throwable, Set[String]] =
    ZStream.fromChunk(Chunk.fromArray(jar))
      .via(ZipUnarchiver.unarchive)
      .map(_._1.name)
      .runCollect
      .map(_.toSet)

  def spec = suite("Deploy")(
    test("redeploy of an already-published version derives a +1 release with a stripped path prefix") {
      withRedeploy { (deployWebJar, allDeployables, capturing) =>
        val gav = MavenCentral.gav("org.webjars", "swagger-ui", "v5.15.1")
        // Pretend MC already has 5.15.1 published so the auto-bump kicks in.
        val existing = Set("5.15.1")
        for
          _ <- Deploy.streamRedeploy(gav, allDeployables, deployWebJar, existing).runDrain
          maybeCaptured <- capturing.captured
          _ = ()
          (publishedGav, jar, pom) = maybeCaptured.getOrElse(throw new AssertionError("publish was never called"))
          jarNames <- jarEntryNames(jar)
        yield assertTrue(
          // GAV in the publish call has the +1 build metadata.
          publishedGav.version.toString == "5.15.1+1",
          publishedGav.artifactId.toString == "swagger-ui",
          publishedGav.groupId.toString == "org.webjars",
          // POM reflects the same release version.
          pom.contains("<version>5.15.1+1</version>"),
          pom.contains("<artifactId>swagger-ui</artifactId>"),
          // In-jar resource path strips the `+1` build metadata so consumers
          // still resolve assets at the canonical 5.15.1 location.
          jarNames.exists(_.startsWith("META-INF/resources/webjars/swagger-ui/5.15.1/")),
          !jarNames.exists(_.contains("5.15.1+1")),
          // pom.xml is in the jar at the +1 path (Maven's own convention is
          // un-affected — only the `META-INF/resources/webjars/...` path
          // strips the build metadata).
          jarNames.contains("META-INF/maven/org.webjars/swagger-ui/pom.xml"),
        )
      }
    },
    test("redeploy of a not-yet-published version uses the upstream version unchanged") {
      withRedeploy { (deployWebJar, allDeployables, capturing) =>
        val gav = MavenCentral.gav("org.webjars", "swagger-ui", "v5.15.1")
        for
          _ <- Deploy.streamRedeploy(gav, allDeployables, deployWebJar, existingVersions = Set.empty).runDrain
          maybeCaptured <- capturing.captured
          (publishedGav, jar, pom) = maybeCaptured.getOrElse(throw new AssertionError("publish was never called"))
          jarNames <- jarEntryNames(jar)
        yield assertTrue(
          publishedGav.version.toString == "5.15.1",
          pom.contains("<version>5.15.1</version>"),
          jarNames.exists(_.startsWith("META-INF/resources/webjars/swagger-ui/5.15.1/")),
        )
      }
    },
    test("redeploy fails fast when the groupId isn't a known deployable") {
      withRedeploy { (deployWebJar, allDeployables, _) =>
        val gav = MavenCentral.gav("org.unknown", "x", "1.0")
        Deploy
          .streamRedeploy(gav, allDeployables, deployWebJar, existingVersions = Set.empty)
          .runDrain
          .exit
          .map { exit =>
            assertTrue(exit.isFailure)
          }
      }
    },
  ).provide(Client.default, TestInfrastructure.sharedRedisLayer) @@
    TestAspect.withLiveClock @@
    TestAspect.timeout(300.seconds)
