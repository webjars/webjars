package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.*
import webjars.TestInfrastructure.{MockMavenCentralDeployer, testConfig}
import zio.*
import zio.http.Client
import zio.test.*

/** Real-network integration: drive DeployJobs end-to-end against the live
 *  npm registry **and** Maven Central (the `fetchPom` pre-flight that
 *  checks whether the GAV is already published is real). Only the final
 *  publish step is mocked, so nothing is actually deployed. The 3d-view
 *  package has a handful of npm deps, exercising the deps-graph iteration. */
object DeployJobsIntegrationSpec extends ZIOSpecDefault:

  private def withDeployJobs[A](f: (DeployJobs[Any], NPM) => ZIO[Scope & Client & zio.redis.Redis & MavenCentral.MavenCentralRepo, Throwable, A]): ZIO[Client & zio.redis.Redis & MavenCentral.MavenCentralRepo, Throwable, A] =
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
        val mavenCentralWebJars = MavenCentralWebJarsLive(config, webJarsFileService, AllDeployablesLive(classic, npm), TestInfrastructure.noopSearchIndex)
        val deployWebJar: DeployWebJar[Any] = DeployWebJarLive[Any](mavenCentralWebJars, mavenCentralDeployer, sourceLocator)
        val jobsLayer =
          (ZLayer.succeed[DeployWebJar[Any]](deployWebJar) ++
            TestInfrastructure.noopDeployFailureTrackerLayer) >>>
            DeployJobs.live[Any]
        jobsLayer.build.flatMap { env =>
          f(env.get[DeployJobs[Any]], npm)
        }
      }
    }

  def spec = suite("DeployJobs integration")(
    test("deploy npm 3d-view 2.0.1 with dependencies") {
      withDeployJobs { (jobs, npm) =>
        jobs.deploy(npm, "3d-view", "2.0.1").runCollect.map { output =>
          val all = output.mkString("\n")
          assertTrue(
            output.exists(_.contains("Determining dependency graph")),
            output.exists(_.contains("Deploying these dependencies:")),
            all.contains("GroupID = org.webjars.npm"),
            all.contains("ArtifactID = 3d-view"),
            all.contains("Version = 2.0.1"),
            all.contains("Deployed!"),
          )
        }
      }
    },
  ).provide(Client.default, TestInfrastructure.sharedRedisLayer, MavenCentral.MavenCentralRepo.live) @@ TestAspect.withLiveClock @@ TestAspect.timeout(300.seconds)
