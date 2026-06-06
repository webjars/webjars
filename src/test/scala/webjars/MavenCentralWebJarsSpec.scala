package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.models.WebJarVersion
import webjars.utils.*
import webjars.utils.WebJarsCache.WebJarMeta
import zio.*
import zio.redis.Redis
import zio.test.*

import java.time.ZonedDateTime

/** Unit-level tests for the numFiles bookkeeping in
 *  [[MavenCentralWebJars]] — specifically the backfill pass that
 *  repairs cached versions stuck at `numFiles = None`.
 *
 *  We don't exercise [[MavenCentralWebJars.refreshAll]] or
 *  [[refreshGroup]] here — those hit real Maven Central and are covered
 *  by [[RefresherIntegrationSpec]] (env-gated). The contract under test
 *  is local: given a Redis cache with some `None` versions, does the
 *  backfill fill the right ones and tolerate per-version failures? */
object MavenCentralWebJarsSpec extends ZIOSpecDefault:

  // Each test uses its own groupId so the shared Valkey container's
  // hash doesn't bleed state between tests. We can't easily flush
  // between tests because other suites share the same Redis layer.
  private def groupFor(testName: String) = MavenCentral.GroupId(s"org.webjars.test.$testName")

  private def artifact(g: MavenCentral.GroupId, name: String) = MavenCentral.GroupArtifact(g, MavenCentral.ArtifactId(name))

  /** A `WebJarsFileService` that returns numFiles deterministically
   *  based on the GAV's version string, optionally failing for some.
   *  All callers are recorded in `callsRef` so tests can assert which
   *  versions were actually queried (e.g. that already-filled versions
   *  are skipped). */
  private class FakeFileService(
    callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]],
    failFor: Set[String] = Set.empty,
    numFilesFor: String => Int = _ => 42,
  ) extends WebJarsFileService:

    def getFileList(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, List[String]] =
      ZIO.fail(new UnsupportedOperationException("getFileList not used in MavenCentralWebJarsSpec"))

    def getNumFiles(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Int] =
      callsRef.update(_ :+ gav) *> {
        if failFor.contains(gav.version.toString) then
          ZIO.fail(new RuntimeException(s"simulated file-service failure for ${gav.version}"))
        else
          ZIO.succeed(numFilesFor(gav.version.toString))
      }

  /** Build the unit under test wired to a recording fake. We construct
   *  the live impl directly because the trait method we exercise
   *  (`refreshMissingNumFiles`) doesn't need `MavenCentralRepo` — it
   *  reads from Redis and calls the file service. */
  private def buildMC(groupId: MavenCentral.GroupId, callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]], failFor: Set[String], numFilesFor: String => Int): MavenCentralWebJars =
    MavenCentralWebJarsLive(
      TestInfrastructure.testConfig,
      FakeFileService(callsRef, failFor = failFor, numFilesFor = numFilesFor),
      new AllDeployables:
        def fromGroupId(g: MavenCentral.GroupId)  = None
        def fromName(n: String)                   = None
        def groupIds(): Set[MavenCentral.GroupId] = Set(groupId)
      ,
      TestInfrastructure.noopSearchIndex,
      TestInfrastructure.noopPopularRanking,
    )

  /** Helper: write a known artifact into the cache with the given
   *  versions (each with optional numFiles). */
  private def seed(ga: MavenCentral.GroupArtifact, versions: List[(String, Option[Int])]): ZIO[Redis, Throwable, Unit] =
    val webJarVersions = versions.map { case (n, nf) => WebJarVersion(n, nf) }
    WebJarsCache.setArtifactDetails(
      ga,
      WebJarMeta("test-name", "https://example.test", webJarVersions, ZonedDateTime.now()),
    )

  /** Helper: read the versions back, in the order the cache stored them. */
  private def readVersions(ga: MavenCentral.GroupArtifact): ZIO[Redis, Throwable, List[WebJarVersion]] =
    WebJarsCache.getArtifact(ga).map(_.map(_.versions).getOrElse(List.empty))

  def spec = suite("MavenCentralWebJarsSpec")(

    test("refreshMissingNumFiles fills versions whose numFiles is None") {
      val groupId = groupFor("fills-none")
      val ga      = artifact(groupId, "a")
      for
        callsRef <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc        = buildMC(groupId, callsRef, failFor = Set.empty, numFilesFor = _ => 42)
        _        <- seed(ga, List("1.0.0" -> None, "1.1.0" -> None, "2.0.0" -> None))
        _        <- mc.refreshMissingNumFiles(groupId)
        calls    <- callsRef.get
        versions <- readVersions(ga)
      yield assertTrue(
        // every version got a number filled in
        versions.flatMap(_.numFiles).size == 3,
        // and each of the seeded versions was queried exactly once
        calls.map(_.version.toString).toSet == Set("1.0.0", "1.1.0", "2.0.0"),
        calls.size == 3,
      )
    },

    test("refreshMissingNumFiles leaves already-filled versions untouched") {
      val groupId = groupFor("skips-filled")
      val ga      = artifact(groupId, "a")
      for
        callsRef <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc        = buildMC(groupId, callsRef, failFor = Set.empty, numFilesFor = _ => 999)
        _        <- seed(ga, List("1.0.0" -> Some(123), "1.1.0" -> None, "2.0.0" -> Some(456)))
        _        <- mc.refreshMissingNumFiles(groupId)
        calls    <- callsRef.get
        versions <- readVersions(ga)
      yield assertTrue(
        // only the None one got queried
        calls.map(_.version.toString) == Vector("1.1.0"),
        // the already-set values are preserved (not overwritten with 999)
        versions.find(_.number == "1.0.0").flatMap(_.numFiles).contains(123),
        versions.find(_.number == "2.0.0").flatMap(_.numFiles).contains(456),
        // and the previously-None one was filled with the fake's value
        versions.find(_.number == "1.1.0").flatMap(_.numFiles).contains(999),
      )
    },

    test("refreshMissingNumFiles tolerates a per-version failure without poisoning siblings") {
      val groupId = groupFor("tolerates-failure")
      val ga      = artifact(groupId, "a")
      for
        callsRef <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc        = buildMC(groupId, callsRef, failFor = Set("1.1.0"), numFilesFor = _ => 7)
        _        <- seed(ga, List("1.0.0" -> None, "1.1.0" -> None, "2.0.0" -> None))
        _        <- mc.refreshMissingNumFiles(groupId)
        versions <- readVersions(ga)
        ok        = versions.filter(_.numFiles.isDefined).map(_.number).toSet
        stuck     = versions.filter(_.numFiles.isEmpty).map(_.number).toSet
      yield assertTrue(
        ok == Set("1.0.0", "2.0.0"),
        stuck == Set("1.1.0"),
      )
    },

    test("refreshMissingNumFiles is a no-op when every version already has numFiles") {
      val groupId = groupFor("noop-when-full")
      val ga      = artifact(groupId, "a")
      for
        callsRef <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc        = buildMC(groupId, callsRef, failFor = Set.empty, numFilesFor = _ => 42)
        _        <- seed(ga, List("1.0.0" -> Some(10), "1.1.0" -> Some(20)))
        _        <- mc.refreshMissingNumFiles(groupId)
        calls    <- callsRef.get
        versions <- readVersions(ga)
      yield assertTrue(
        calls.isEmpty,
        versions.find(_.number == "1.0.0").flatMap(_.numFiles).contains(10),
        versions.find(_.number == "1.1.0").flatMap(_.numFiles).contains(20),
      )
    },

  ).provide(
    TestInfrastructure.sharedRedisLayer,
    zio.http.Client.default,
  ) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
