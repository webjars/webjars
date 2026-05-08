package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.models.WebJar
import webjars.utils.*
import webjars.TestInfrastructure.testConfig
import zio.*
import zio.http.Client
import zio.test.*

import java.io.FileNotFoundException
import scala.xml.Elem

object DeployWebJarSpec extends ZIOSpecDefault:

  private def withDeploy[A](f: (DeployWebJar, NPM, Classic) => ZIO[Scope, Throwable, A]): ZIO[Client, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val config = testConfig.copy(ossDisableDeploy = true)
        val cache = CacheLive()
        val valkey = ValkeyLive()
        val git = GitLive(client)
        val gitHub = GitHubLive(client, config, cache)
        val semVer = SemVerLive(client)
        val maven = MavenLive(git, semVer)
        val licenseDetector = LicenseDetectorLive(client, testConfig.githubAuthToken)
        val sourceLocator = SourceLocatorLive(client, git)
        val webJarsFileService = WebJarsFileServiceLive(client, config)
        val npm = NPMLive(client, licenseDetector, git, gitHub, maven, semVer)
        val classic = ClassicLive(client, licenseDetector, gitHub, cache, config, npm)
        val allDeployables = AllDeployablesLive(classic, npm)
        val mavenCentralDeployer = MockMavenCentralDeployer()
        val mavenCentralWebJars = MockMavenCentralWebJars(config, webJarsFileService, valkey, allDeployables)
        val heroku = HerokuLive(client, config)
        val deployWebJar = DeployWebJarLive(mavenCentralWebJars, mavenCentralDeployer, sourceLocator, config, heroku)
        f(deployWebJar, npm, classic)
      }
    }

  def spec = suite("DeployWebJar")(
    test("work with npm") {
      withDeploy { (deployWebJar, npm, _) =>
        deployWebJar.deploy(npm, "jquery", "3.2.1", false, true, false).runCollect.map { output =>
          val last = output.lastOption.getOrElse("")
          assertTrue(
            last.contains("GroupID = org.webjars.npm"),
            last.contains("ArtifactID = jquery"),
            last.contains("Version = 3.2.1"),
          )
        }
      }
    },
    test("work with Classic") {
      withDeploy { (deployWebJar, _, classic) =>
        deployWebJar.deploy(classic, "swagger-ui", "v5.15.1", false, true, false).runCollect.map { output =>
          val all = output.mkString
          assertTrue(
            all.contains("Resolved Licenses: Apache-2.0"),
            all.contains("GroupID = org.webjars"),
            all.contains("ArtifactID = swagger-ui"),
            all.contains("Version = 5.15.1"),
            all.contains("Deployed!"),
          )
        }
      }
    },
    test("deploy deps") {
      withDeploy { (deployWebJar, npm, _) =>
        deployWebJar.deploy(npm, "react", "16.8.6", true, true, false).runCollect.map { output =>
          assertTrue(output.exists(_.contains("Deploying these dependencies:")))
        }
      }
    },
  ).provide(Client.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(300.seconds)

class MockMavenCentralDeployer extends MavenCentralDeployer:
  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Any, Throwable, Unit] =
    ZIO.unit

class MockMavenCentralWebJars(config: webjars.config.AppConfig, webJarsFileService: WebJarsFileService, valkey: Valkey, allDeployables: AllDeployables)
  extends MavenCentralWebJarsLive(config, webJarsFileService, valkey, allDeployables):
  override def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Elem] =
    ZIO.fail(new FileNotFoundException("no mock pom"))
