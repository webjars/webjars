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
   *  are skipped).
   *
   *  Three flavours of failure are supported, matching what the real
   *  service emits:
   *    - `notFoundFor`: returns `WebJarNotFoundException` (a
   *      `FileNotFoundException` subtype with the
   *      [[WebJarsFileService.UnusableWebJarException]] marker — the
   *      "jar isn't on Maven Central" signal).
   *    - `unprocessableFor`: returns `WebJarUnprocessableException`
   *      (also a `UnusableWebJarException` — the "jar is on Maven
   *      Central but is corrupt" signal, surfaced as a 422 by the
   *      real file-service).
   *    - `failFor`: returns a generic `RuntimeException` (transient
   *      failure — should NOT tombstone). */
  private class FakeFileService(
    callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]],
    failFor: Set[String] = Set.empty,
    notFoundFor: Set[String] = Set.empty,
    unprocessableFor: Set[String] = Set.empty,
    numFilesFor: String => Int = _ => 42,
  ) extends WebJarsFileService:

    def getFileList(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, List[String]] =
      ZIO.fail(new UnsupportedOperationException("getFileList not used in MavenCentralWebJarsSpec"))

    def getNumFiles(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Int] =
      callsRef.update(_ :+ gav) *> {
        if notFoundFor.contains(gav.version.toString) then
          ZIO.fail(new WebJarsFileService.WebJarNotFoundException(s"simulated 404 for ${gav.version}"))
        else if unprocessableFor.contains(gav.version.toString) then
          ZIO.fail(new WebJarsFileService.WebJarUnprocessableException(s"simulated 422 for ${gav.version}"))
        else if failFor.contains(gav.version.toString) then
          ZIO.fail(new RuntimeException(s"simulated transient failure for ${gav.version}"))
        else
          ZIO.succeed(numFilesFor(gav.version.toString))
      }

  /** Build the unit under test wired to a recording fake. We construct
   *  the live impl directly because the trait method we exercise
   *  (`refreshMissingNumFiles`) doesn't need `MavenCentralRepo` — it
   *  reads from Redis and calls the file service. */
  private def buildMC(
    groupId: MavenCentral.GroupId,
    callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]],
    failFor: Set[String] = Set.empty,
    notFoundFor: Set[String] = Set.empty,
    unprocessableFor: Set[String] = Set.empty,
    numFilesFor: String => Int,
  ): MavenCentralWebJars =
    MavenCentralWebJarsLive(
      TestInfrastructure.testConfig,
      FakeFileService(callsRef, failFor = failFor, notFoundFor = notFoundFor, unprocessableFor = unprocessableFor, numFilesFor = numFilesFor),
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

    test("refreshMissingNumFiles tombstones a ghost version on FileNotFoundException AND removes it from the cache") {
      // Canonical case: `org.webjars.npm:killable:1.0.1` — Maven Central
      // metadata lists 1.0.1 but the directory only has the .pom (no jar),
      // so the file-service returns 404 (FileNotFoundException). We
      // distinguish this signal from generic Throwable failures so that
      // permanent missing-jar versions get pruned instead of retried
      // every cycle forever.
      val groupId = groupFor("tombstones-ghost")
      val ga      = artifact(groupId, "killable")
      for
        callsRef     <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc            = buildMC(groupId, callsRef, notFoundFor = Set("1.0.1"), numFilesFor = _ => 7)
        _            <- seed(ga, List("1.0.0" -> None, "1.0.1" -> None))
        _            <- mc.refreshMissingNumFiles(groupId)
        versions     <- readVersions(ga)
        tombstones   <- WebJarsCache.getVersionTombstones(ga)
      yield assertTrue(
        // 1.0.1 is gone from the cached versions list (won't show up in
        // search/popular/all/list/artifact-detail).
        versions.map(_.number).toSet == Set("1.0.0"),
        // 1.0.0 succeeded with the fake's numFiles.
        versions.find(_.number == "1.0.0").flatMap(_.numFiles).contains(7),
        // The ghost is in the version-tombstone set so the next cycle
        // skips it instead of re-fetching numFiles.
        tombstones == Set("1.0.1"),
      )
    },

    test("refreshMissingNumFiles tombstones a corrupt-jar version on 422 (WebJarUnprocessableException)") {
      // The file-service returns 422 when the jar IS published but
      // its contents can't be processed (corrupt zip, encoding bug,
      // etc.). Same tombstone semantics as 404 — the version is
      // permanently unusable as a webjar — but a different cause.
      // Both 404 and 422 share the `UnusableWebJarException` marker
      // so the cache cleanup pattern-match treats them identically.
      val groupId = groupFor("tombstones-corrupt")
      val ga      = artifact(groupId, "browserify-aes")
      for
        callsRef     <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc            = buildMC(groupId, callsRef, unprocessableFor = Set("1.2.0"), numFilesFor = _ => 13)
        _            <- seed(ga, List("1.1.0" -> None, "1.2.0" -> None))
        _            <- mc.refreshMissingNumFiles(groupId)
        versions     <- readVersions(ga)
        tombstones   <- WebJarsCache.getVersionTombstones(ga)
      yield assertTrue(
        versions.map(_.number).toSet == Set("1.1.0"),
        versions.find(_.number == "1.1.0").flatMap(_.numFiles).contains(13),
        tombstones == Set("1.2.0"),
      )
    },

    test("WebJarNotFoundException IS-A FileNotFoundException — protects existing consumer pattern matches") {
      // Some callers (e.g. AppRoutes' previous catchAll) match on
      // `FileNotFoundException` directly. Keeping the inheritance
      // invariant means those don't break when we route 404s through
      // `WebJarNotFoundException` instead. Verify it explicitly so a
      // future refactor doesn't silently regress.
      val ex = new WebJarsFileService.WebJarNotFoundException("test")
      assertTrue(
        ex.isInstanceOf[java.io.FileNotFoundException],
        ex.isInstanceOf[WebJarsFileService.UnusableWebJarException],
      )
    },

    test("WebJarUnprocessableException IS-A UnusableWebJarException but NOT a FileNotFoundException") {
      // 422 is semantically distinct from 404 ("file exists but is
      // corrupt" vs "file doesn't exist"). The marker trait unifies
      // their tombstone treatment without forcing a FileNotFoundException
      // identity that would mislead consumers that legitimately care
      // about the distinction.
      val ex = new WebJarsFileService.WebJarUnprocessableException("test")
      assertTrue(
        !ex.isInstanceOf[java.io.FileNotFoundException],
        ex.isInstanceOf[WebJarsFileService.UnusableWebJarException],
      )
    },

    test("refreshMissingNumFiles does NOT tombstone on a generic (transient) failure") {
      // Critical to distinguish from the previous test: a generic
      // `Throwable` (5xx, transport error, parse error) is treated as
      // transient. The version stays in cache with `numFiles=None` so
      // the next cycle retries. NOT added to the version-tombstone set.
      val groupId = groupFor("no-tombstone-on-transient")
      val ga      = artifact(groupId, "a")
      for
        callsRef     <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc            = buildMC(groupId, callsRef, failFor = Set("1.0.0"), numFilesFor = _ => 99)
        _            <- seed(ga, List("1.0.0" -> None, "1.1.0" -> None))
        _            <- mc.refreshMissingNumFiles(groupId)
        versions     <- readVersions(ga)
        tombstones   <- WebJarsCache.getVersionTombstones(ga)
      yield assertTrue(
        // 1.0.0 still in cache (not removed; will retry next cycle).
        versions.map(_.number).toSet == Set("1.0.0", "1.1.0"),
        // 1.0.0 still has numFiles=None.
        versions.find(_.number == "1.0.0").flatMap(_.numFiles).isEmpty,
        // 1.1.0 succeeded with the fake's value.
        versions.find(_.number == "1.1.0").flatMap(_.numFiles).contains(99),
        // No tombstone written — the next cycle will retry the
        // transient failure.
        tombstones.isEmpty,
      )
    },

    test("refreshMissingNumFiles is idempotent on repeated ghost-detection") {
      // Once a version is tombstoned, a subsequent run of the same
      // backfill shouldn't re-add it to the cached versions list, and
      // shouldn't repeatedly fetch numFiles for it. This is the key
      // property: ghost versions don't keep generating WARN log noise
      // every cycle.
      val groupId = groupFor("idempotent-ghost")
      val ga      = artifact(groupId, "killable")
      for
        callsRef    <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        mc           = buildMC(groupId, callsRef, notFoundFor = Set("1.0.1"), numFilesFor = _ => 5)
        _           <- seed(ga, List("1.0.0" -> None, "1.0.1" -> None))
        // First run: detects and tombstones 1.0.1.
        _           <- mc.refreshMissingNumFiles(groupId)
        afterFirst  <- callsRef.get.map(_.size)
        // Second run: 1.0.1 should already be gone from the cache,
        // and the cached 1.0.0 already has numFiles, so no calls to
        // file-service.
        _           <- mc.refreshMissingNumFiles(groupId)
        afterSecond <- callsRef.get.map(_.size)
        versions    <- readVersions(ga)
      yield assertTrue(
        // First run: hits 1.0.0 + 1.0.1 once each.
        afterFirst == 2,
        // Second run: zero new calls (cache is fully resolved + ghost
        // was removed).
        afterSecond == afterFirst,
        // Cache reflects the cleanup.
        versions.map(_.number).toSet == Set("1.0.0"),
      )
    },

    test("setArtifactDetails refuses to write entries with empty artifactId") {
      // Defensive guard against the historical bug where pre-0.9.x
      // zio-mavencentral parsing leaked `<a href="/">` directory
      // listings through as empty artifactIds. Even if the discovery
      // boundary regresses, the cache itself should never accept the
      // bad data.
      val groupId = groupFor("rejects-empty-aid")
      val badGa   = MavenCentral.GroupArtifact(groupId, MavenCentral.ArtifactId(""))
      val meta    = WebJarMeta("name", "https://example.test", List(WebJarVersion("1.0.0", None)), ZonedDateTime.now())
      for
        result <- WebJarsCache.setArtifactDetails(badGa, meta).exit
      yield assertTrue(
        // We use ZIO.die for this, since callers are NOT expected to
        // handle it — empty artifactId is a programmer error, not a
        // recoverable failure.
        result.isFailure,
      )
    },

    test("cleanupCachedArtifacts removes empty-artifactId entries directly via Redis") {
      // Empty-artifactId entries can't go through setArtifactDetails
      // (the guard above rejects them), so we plant one via raw HSET
      // to simulate the legacy stale data we found in production.
      val groupId   = groupFor("cleanup-empty-aid")
      val realArt   = artifact(groupId, "real")
      val realMeta  = WebJarMeta("real", "https://example.test", List(WebJarVersion("1.0.0", Some(5))), ZonedDateTime.now())
      val emptyMeta = WebJarMeta("", "https://github.com/webjars/", List(WebJarVersion("5.0.0", None)), ZonedDateTime.now())

      // Test scaffolding: a MavenCentralWebJarsLive that returns a
      // synthetic <packaging>jar</packaging> POM for everything, so
      // `cleanupCachedArtifacts`'s Phase 2 (parent-POM check)
      // doesn't accidentally remove the real artifact in this test.
      def jarOnlyMC(callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]]) = new MavenCentralWebJarsLive(
        TestInfrastructure.testConfig,
        FakeFileService(callsRef),
        new AllDeployables:
          def fromGroupId(g: MavenCentral.GroupId)  = None
          def fromName(n: String)                   = None
          def groupIds(): Set[MavenCentral.GroupId] = Set(groupId)
        ,
        TestInfrastructure.noopSearchIndex,
        TestInfrastructure.noopPopularRanking,
      ):
        override def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentral.MavenCentralRepo, Throwable, scala.xml.Elem] =
          ZIO.succeed(<project><artifactId>{gav.artifactId.toString}</artifactId><packaging>jar</packaging></project>)

      for
        callsRef <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        // Plant a normal entry through the legitimate API.
        _        <- WebJarsCache.setArtifactDetails(realArt, realMeta)
        // Plant an empty-artifactId entry via raw HSET (bypassing the
        // guard) to simulate stale data from before 0.9.x.
        _        <- ZIO.serviceWithZIO[Redis](_.hSet(groupId.toString, "" -> emptyMeta).unit)
        // Sanity-check that both are present before cleanup.
        before   <- ZIO.serviceWithZIO[Redis](_.hKeys(groupId.toString).returning[String]).map(_.toSet)
        // Run cleanup.
        cleaned  <- jarOnlyMC(callsRef).cleanupCachedArtifacts(groupId)
        after    <- ZIO.serviceWithZIO[Redis](_.hKeys(groupId.toString).returning[String]).map(_.toSet)
      yield assertTrue(
        before.contains(""),
        before.contains("real"),
        cleaned == 1,
        // Empty key is gone, real key survived.
        !after.contains(""),
        after.contains("real"),
      )
    },

    test("cleanupCachedArtifacts removes pom-packaged entries and tombstones them") {
      val groupId    = groupFor("cleanup-pom-pkg")
      val parentArt  = artifact(groupId, "when-parent")
      val realArt    = artifact(groupId, "jquery")
      val parentMeta = WebJarMeta("When Parent", "https://example.test", List(WebJarVersion("3.5.2", None)), ZonedDateTime.now())
      val realMeta   = WebJarMeta("jQuery", "https://example.test", List(WebJarVersion("3.7.1", Some(8))), ZonedDateTime.now())

      // Scaffolding: a MavenCentralWebJarsLive that returns
      // <packaging>pom</packaging> for `when-parent` and
      // <packaging>jar</packaging> for everything else. This is the
      // exact distinction we found in production for org.webjars.
      def pomAwareMC(callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]]) = new MavenCentralWebJarsLive(
        TestInfrastructure.testConfig,
        FakeFileService(callsRef),
        new AllDeployables:
          def fromGroupId(g: MavenCentral.GroupId)  = None
          def fromName(n: String)                   = None
          def groupIds(): Set[MavenCentral.GroupId] = Set(groupId)
        ,
        TestInfrastructure.noopSearchIndex,
        TestInfrastructure.noopPopularRanking,
      ):
        override def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentral.MavenCentralRepo, Throwable, scala.xml.Elem] =
          val packaging = if gav.artifactId.toString.endsWith("-parent") then "pom" else "jar"
          ZIO.succeed(<project><artifactId>{gav.artifactId.toString}</artifactId><packaging>{packaging}</packaging></project>)

      for
        callsRef   <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        _          <- WebJarsCache.setArtifactDetails(parentArt, parentMeta)
        _          <- WebJarsCache.setArtifactDetails(realArt, realMeta)
        cleaned    <- pomAwareMC(callsRef).cleanupCachedArtifacts(groupId)
        after      <- ZIO.serviceWithZIO[Redis](_.hKeys(groupId.toString).returning[String]).map(_.toSet)
        tombstones <- WebJarsCache.getTombstones(groupId)
      yield assertTrue(
        cleaned == 1,
        // Parent POM is gone from the hash AND tombstoned.
        !after.contains("when-parent"),
        tombstones.contains(MavenCentral.ArtifactId("when-parent")),
        // Real artifact survived and is NOT tombstoned.
        after.contains("jquery"),
        !tombstones.contains(MavenCentral.ArtifactId("jquery")),
      )
    },

    test("cleanupCachedArtifacts is idempotent — second run is a no-op") {
      // Ensures running cleanup repeatedly (e.g. if the gate flag's
      // TTL expires) doesn't accidentally delete more than it should.
      val groupId = groupFor("cleanup-idempotent")
      val ga      = artifact(groupId, "a")
      val meta    = WebJarMeta("a", "https://example.test", List(WebJarVersion("1.0.0", Some(3))), ZonedDateTime.now())

      def jarOnlyMC(callsRef: Ref[Vector[MavenCentral.GroupArtifactVersion]]) = new MavenCentralWebJarsLive(
        TestInfrastructure.testConfig,
        FakeFileService(callsRef),
        new AllDeployables:
          def fromGroupId(g: MavenCentral.GroupId)  = None
          def fromName(n: String)                   = None
          def groupIds(): Set[MavenCentral.GroupId] = Set(groupId)
        ,
        TestInfrastructure.noopSearchIndex,
        TestInfrastructure.noopPopularRanking,
      ):
        override def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[MavenCentral.MavenCentralRepo, Throwable, scala.xml.Elem] =
          ZIO.succeed(<project><artifactId>{gav.artifactId.toString}</artifactId><packaging>jar</packaging></project>)

      for
        callsRef <- Ref.make(Vector.empty[MavenCentral.GroupArtifactVersion])
        _        <- WebJarsCache.setArtifactDetails(ga, meta)
        mc        = jarOnlyMC(callsRef)
        first    <- mc.cleanupCachedArtifacts(groupId)
        second   <- mc.cleanupCachedArtifacts(groupId)
        after    <- ZIO.serviceWithZIO[Redis](_.hKeys(groupId.toString).returning[String]).map(_.toSet)
      yield assertTrue(
        first == 0,
        second == 0,
        after.contains("a"),
      )
    },

  ).provide(
    TestInfrastructure.sharedRedisLayer,
    zio.http.Client.default,
    MavenCentral.MavenCentralRepo.live,
  ) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
