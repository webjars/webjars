package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.*
import zio.*
import zio.direct.*
import zio.http.URL
import zio.stream.ZStream
import zio.test.*

object DeployJobsSpec extends ZIOSpecDefault:

  /** A fake Deployable. Only `name` is read by DeployJobs when
   *  `deployDependencies = false`, so the rest are stubbed out. */
  private object FakeDeployable extends Deployable:
    val licenseDetector: LicenseDetector = null
    val name: String = "fake"
    val groupId: MavenCentral.GroupId = MavenCentral.GroupId("org.webjars.fake")
    val metadataFile: Option[String] = None
    def artifactId(nameOrUrlish: String): ZIO[Scope, Throwable, MavenCentral.ArtifactId] = ZIO.dieMessage("not used")
    def excludes(nameOrUrlish: String): ZIO[Scope, Throwable, Set[String]] = ZIO.dieMessage("not used")
    def maybeBaseDirGlob(nameOrUrlish: String): ZIO[Scope, Throwable, Option[String]] = ZIO.dieMessage("not used")
    def info(nameOrUrlish: String, version: String, maybeSourceUri: Option[URL] = None): ZIO[Scope, Throwable, PackageInfo] = ZIO.dieMessage("not used")
    def mavenDependencies(dependencies: Map[String, String]): ZIO[Scope, Throwable, Set[(MavenCentral.GroupArtifact, String)]] = ZIO.dieMessage("not used")
    def archive(nameOrUrlish: String, version: String): ZIO[Scope, Throwable, java.io.InputStream] = ZIO.dieMessage("not used")
    def file(nameOrUrlish: String, version: String, filename: String): ZIO[Scope, Throwable, String] = ZIO.dieMessage("not used")
    def versions(nameOrUrlish: String): ZIO[Scope, Throwable, Set[String]] = ZIO.dieMessage("not used")
    def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty): ZIO[Scope, Throwable, Map[String, String]] = ZIO.dieMessage("not used")

  /** A DeployWebJar that emits a fixed message sequence, slowly.
   *  `Env = Any` because no Sonatype is needed. */
  private class FakeDeployWebJar(messages: List[String], gate: Promise[Nothing, Unit], runs: Ref[Int]) extends DeployWebJar[Any]:
    def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope, Throwable, String] =
      ZStream.fromZIO(runs.update(_ + 1)).drain ++
        ZStream.fromIterable(messages).tap(_ => ZIO.unit) ++
        ZStream.fromZIO(gate.await).drain ++
        ZStream.succeed("Deployed!")
    def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Array[Byte])] =
      ZIO.dieMessage("not used")

  private val messages = List("m1", "m2", "m3")

  def spec = suite("DeployJobs")(
    test("late subscriber sees full stream from the beginning") {
      defer:
        val gate = Promise.make[Nothing, Unit].run
        val runs = Ref.make(0).run
        val fake = FakeDeployWebJar(messages, gate, runs)
        val jobs = DeployJobs.live[Any].build.provide(ZLayer.succeed[DeployWebJar[Any]](fake), Scope.default).run.get[DeployJobs[Any]]

        // First subscriber: pull the first message, then a second subscriber attaches.
        val sub1Fiber = jobs.deploy(FakeDeployable, "pkg", "1.0", deployDependencies = false).runCollect.fork.run

        // Wait until the producer has emitted m1..m3 (everything except the final "Deployed!").
        // We poll the subscriber's progress indirectly via a small delay.
        ZIO.sleep(200.millis).run

        // Late subscriber attaches — should see ALL messages including the ones already emitted.
        val sub2Fiber = jobs.deploy(FakeDeployable, "pkg", "1.0", deployDependencies = false).runCollect.fork.run

        // Now let the producer finish.
        gate.succeed(()).run

        val out1 = sub1Fiber.join.run
        val out2 = sub2Fiber.join.run
        val runCount = runs.get.run

        assertTrue(
          out1.toList == (messages :+ "Deployed!"),
          out2.toList == (messages :+ "Deployed!"),
          runCount == 1, // producer ran exactly once even with two subscribers
        )
    },
    test("concurrent deploys of same key share a single producer run") {
      defer:
        val gate = Promise.make[Nothing, Unit].run
        val runs = Ref.make(0).run
        val fake = FakeDeployWebJar(messages, gate, runs)
        val jobs = DeployJobs.live[Any].build.provide(ZLayer.succeed[DeployWebJar[Any]](fake), Scope.default).run.get[DeployJobs[Any]]

        // Spawn N subscribers for the same key. The producer should run once.
        val fibers = ZIO.foreach(1 to 5) { _ =>
          jobs.deploy(FakeDeployable, "pkg", "1.0", deployDependencies = false).runCollect.fork
        }.run

        ZIO.sleep(200.millis).run
        gate.succeed(()).run

        val outs = ZIO.foreach(fibers)(_.join).run
        val runCount = runs.get.run

        assertTrue(
          runCount == 1,
          outs.forall(_.toList == (messages :+ "Deployed!")),
        )
    },
  ).provide(zio.http.Client.default, TestInfrastructure.sharedRedisLayer, MavenCentral.MavenCentralRepo.live) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
