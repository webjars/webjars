package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.*
import webjars.TestInfrastructure.{MockMavenCentralDeployer, testConfig}
import zio.*
import zio.http.Client
import zio.test.*

import java.io.FileNotFoundException
import scala.xml.Elem

object DeployWebJarSpec extends ZIOSpecDefault:

  private def withDeploy[A](f: (DeployWebJar[Any], NPM, Classic) => ZIO[Scope & Client & zio.redis.Redis & MavenCentral.MavenCentralRepo, Throwable, A]): ZIO[Client & zio.redis.Redis & MavenCentral.MavenCentralRepo, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val config = testConfig
        val cache = CacheLive()
        val git = GitLive(client)
        val gitHub = GitHubLive(client, config, cache)
        val semVer = SemVerLive(client)
        val maven = MavenLive(git, semVer)
        val sourceLocator = SourceLocatorLive(client, git)
        val webJarsFileService = WebJarsFileServiceLive(client, config)
        val npm = NPMLive(client, git, gitHub, maven, semVer)
        val classic = ClassicLive(client, gitHub, cache, config, npm)
        val mavenCentralDeployer: MavenCentralDeployer[Any] = MockMavenCentralDeployer()
        val mavenCentralWebJars = MockMavenCentralWebJars(config, webJarsFileService, AllDeployablesLive(classic, npm))
        val deployWebJar: DeployWebJar[Any] = DeployWebJarLive[Any](mavenCentralWebJars, mavenCentralDeployer, sourceLocator)
        f(deployWebJar, npm, classic)
      }
    }

  def spec = suite("DeployWebJar")(
    test("work with npm") {
      withDeploy { (deployWebJar, npm, _) =>
        deployWebJar.deploy(npm, "jquery", "3.2.1").runCollect.map { output =>
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
        deployWebJar.deploy(classic, "swagger-ui", "v5.15.1").runCollect.map { output =>
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
  ).provide(Client.default, TestInfrastructure.sharedRedisLayer, MavenCentral.MavenCentralRepo.live) @@ TestAspect.withLiveClock @@ TestAspect.timeout(300.seconds)

class MockMavenCentralWebJars(config: webjars.config.AppConfig, webJarsFileService: WebJarsFileService, allDeployables: AllDeployables)
  extends MavenCentralWebJarsLive(config, webJarsFileService, allDeployables, TestInfrastructure.noopSearchIndex, TestInfrastructure.noopPopularRanking):
  override def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentral.MavenCentralRepo, Throwable, Elem] =
    ZIO.fail(new FileNotFoundException("no mock pom"))
