package webjars

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

  private def withDeployJobs[A](f: (DeployJobs, NPM) => ZIO[Scope, Throwable, A]): ZIO[Client, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val config = testConfig
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
        val mavenCentralDeployer = MockMavenCentralDeployer()
        val mavenCentralWebJars = MavenCentralWebJarsLive(config, webJarsFileService, valkey, AllDeployablesLive(classic, npm))
        val deployWebJar = DeployWebJarLive(mavenCentralWebJars, mavenCentralDeployer, sourceLocator)
        val jobsLayer = ZLayer.succeed[DeployWebJar](deployWebJar) >>> DeployJobs.live
        jobsLayer.build.flatMap { env =>
          f(env.get[DeployJobs], npm)
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
  ).provide(Client.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(300.seconds)
