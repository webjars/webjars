package webjars

import webjars.utils.*
import zio.*
import zio.http.{Response, URL}
import zio.redis.Redis
import zio.test.*
import zio.test.TestAspect

import java.util.concurrent.TimeoutException

object DeployFailureTrackerSpec extends ZIOSpecDefault:

  // ---------------------------------------------------------------- //
  // Fake GitHub: in-memory store of issues, mutable via the trait API. //
  // Lets us assert the tracker's call sequence without hitting the    //
  // real /repos/.../issues endpoint.                                   //
  // ---------------------------------------------------------------- //

  case class FakeIssue(number: Int, title: String, body: String, labels: Set[String], comments: List[String], closed: Boolean)

  case class FakeGitHubState(issues: Map[Int, FakeIssue], nextNumber: Int):
    def withIssue(i: FakeIssue): FakeGitHubState =
      copy(issues = issues + (i.number -> i), nextNumber = math.max(nextNumber, i.number + 1))

  object FakeGitHubState:
    val empty: FakeGitHubState = FakeGitHubState(Map.empty, 1)

  case class FakeGitHub(
    ref: Ref[FakeGitHubState],
    authToken: Option[String] = Some("ghp_FAKE_TOKEN_FOR_TESTS"),
  ) extends GitHub:
    def maybeAuthToken: Option[String] = authToken

    // Operations the tracker doesn't use — stubbed.
    def currentUrls(url: URL): ZIO[Scope, Throwable, (URL, URL, URL)] = ZIO.dieMessage("unused")
    def raw(gitHubUrl: URL, tagCommitOrBranch: String, fileName: String): ZIO[Scope, Throwable, String] = ZIO.dieMessage("unused")
    def allPages[T](path: String, accumulator: Set[T] = Set.empty[T])(mapFunction: Response => ZIO[Scope, Throwable, Set[T]]): ZIO[Scope, Throwable, Set[T]] = ZIO.dieMessage("unused")
    def tags(repo: String): ZIO[Scope, Throwable, Set[String]] = ZIO.dieMessage("unused")

    def findOpenIssueByTitle(repo: GitHub.Repo, title: String, label: String): ZIO[Scope, Throwable, Option[GitHub.Issue]] =
      ref.get.map { state =>
        state.issues.values
          .find(i => !i.closed && i.title == title && i.labels.contains(label))
          .map(i => GitHub.Issue(i.number, URL.unsafeParse(s"https://github.com/${repo.slug}/issues/${i.number}")))
      }

    def createIssue(repo: GitHub.Repo, title: String, body: String, labels: Set[String]): ZIO[Scope, Throwable, GitHub.Issue] =
      ref.modify { state =>
        val n = state.nextNumber
        val issue = FakeIssue(n, title, body, labels, comments = Nil, closed = false)
        val newState = state.copy(issues = state.issues + (n -> issue), nextNumber = n + 1)
        (GitHub.Issue(n, URL.unsafeParse(s"https://github.com/${repo.slug}/issues/$n")), newState)
      }

    def commentOnIssue(repo: GitHub.Repo, issueNumber: Int, body: String): ZIO[Scope, Throwable, Unit] =
      ref.update { state =>
        state.issues.get(issueNumber).fold(state) { issue =>
          state.copy(issues = state.issues + (issueNumber -> issue.copy(comments = issue.comments :+ body)))
        }
      }

    def closeIssue(repo: GitHub.Repo, issueNumber: Int, comment: Option[String]): ZIO[Scope, Throwable, Unit] =
      ref.update { state =>
        state.issues.get(issueNumber).fold(state) { issue =>
          val withComment = comment.fold(issue)(c => issue.copy(comments = issue.comments :+ c))
          state.copy(issues = state.issues + (issueNumber -> withComment.copy(closed = true)))
        }
      }

  // -- Helpers ---------------------------------------------------- //

  private val testRepo = "webjars/webjars"

  private def trackerWith(authToken: Option[String], repoOpt: Option[String]): UIO[(DeployFailureTracker, Ref[FakeGitHubState])] =
    for
      ref     <- Ref.make(FakeGitHubState.empty)
      gitHub   = FakeGitHub(ref, authToken)
      config   = TestInfrastructure.testConfig.copy(deployFailureIssuesRepo = repoOpt)
      tracker  = DeployFailureTracker.DeployFailureTrackerLive(gitHub, config)
    yield (tracker, ref)

  /** Each Redis-touching test gets its own deploy key so the shared
   *  valkey container's `deploy-failures` hash doesn't leak state across
   *  tests run in the same suite. */
  private def keyFor(name: String): DeployFailureTracker.DeployKey =
    DeployFailureTracker.DeployKey(
      com.jamesward.zio_mavencentral.MavenCentral.GroupId("org.webjars.npm"),
      s"test-$name",
      "0.0.0",
    )

  // -- Spec ------------------------------------------------------- //

  def spec = suite("DeployFailureTracker")(
    suite("scrub")(
      test("redacts a classic GitHub token") {
        val cleaned = DeployFailureTracker.scrub("token: ghp_AAAAAAAAAAAAAAAAAAAAAAAAAAA1234567")
        assertTrue(!cleaned.contains("ghp_AAAAAAAAAAAAAAAAAAAAAAAAAAA1234567"))
      },
      test("redacts a fine-grained PAT") {
        val cleaned = DeployFailureTracker.scrub("github_pat_AAAAAAAAAAAAAAAAAAAAAAAAA_BBBBBBBBBBBBBBBBBBBB")
        assertTrue(cleaned.contains("github_pat_REDACTED"))
      },
      test("redacts URL userinfo") {
        val cleaned = DeployFailureTracker.scrub("Could not fetch https://user:secret@example.com/foo")
        assertTrue(
          !cleaned.contains("user:secret"),
          cleaned.contains("REDACTED@example.com/foo"),
        )
      },
      test("redacts Bearer tokens in error bodies") {
        val cleaned = DeployFailureTracker.scrub("Authorization: Bearer abc123def456ghi789")
        assertTrue(!cleaned.contains("abc123def456ghi789"))
      },
      test("leaves innocuous text unchanged") {
        val s = "The Classic WebJar foo could not be deployed because of bar."
        assertTrue(DeployFailureTracker.scrub(s) == s)
      },
    ),
    suite("trackFailure")(
      test("UserInput failure → no issue, no Redis write") {
        val key = keyFor("user-input")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          failure         = DeployFailure.classify(ServerError("not found", 404))
          result         <- tracker.trackFailure(key, failure, Chunk("log line"))
          state          <- ref.get
        yield assertTrue(
          result.isEmpty,
          state.issues.isEmpty,
        )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("Transient failure → no issue") {
        val key = keyFor("transient")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          failure         = DeployFailure.classify(new TimeoutException("slow"))
          result         <- tracker.trackFailure(key, failure, Chunk("log"))
          state          <- ref.get
        yield assertTrue(
          result.isEmpty,
          state.issues.isEmpty,
        )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("Systemic failure with no token → no issue (short-circuit, no error)") {
        val key = keyFor("no-token")
        val program = for
          (tracker, ref) <- trackerWith(authToken = None, Some(testRepo))
          failure         = DeployFailure.classify(LicenseNotFoundException("nope"))
          result         <- tracker.trackFailure(key, failure, Chunk("log"))
          state          <- ref.get
        yield assertTrue(
          result.isEmpty,
          state.issues.isEmpty,
        )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("Systemic failure with no repo configured → no issue") {
        val key = keyFor("no-repo")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), repoOpt = None)
          failure         = DeployFailure.classify(LicenseNotFoundException("nope"))
          result         <- tracker.trackFailure(key, failure, Chunk("log"))
          state          <- ref.get
        yield assertTrue(
          result.isEmpty,
          state.issues.isEmpty,
        )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("Systemic failure → files a new issue and stores the number in Redis") {
        val key = keyFor("new-issue")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          failure         = DeployFailure.classify(LicenseNotFoundException("nope - new-issue 0.0.0"))
          result         <- tracker.trackFailure(key, failure, Chunk("Got package info", "Resolving licenses"))
          state          <- ref.get
          stored         <- ZIO.serviceWithZIO[Redis](_.hGet("deploy-failures", key.asString).returning[String])
        yield
          val maybeIssue = state.issues.values.headOption
          assertTrue(
            result.isDefined,
            state.issues.size == 1,
            maybeIssue.exists(_.title == s"Deploy failed: ${key.asString}"),
            maybeIssue.exists(_.labels.contains("deploy-failure")),
            maybeIssue.exists(_.body.contains("Resolving licenses")),
            maybeIssue.exists(_.body.contains("nope - new-issue 0.0.0")),
            stored.contains(maybeIssue.get.number.toString),
          )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("repeated Systemic failure → comments on existing issue, no duplicate") {
        val key = keyFor("repeat")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          failure         = DeployFailure.classify(LicenseNotFoundException("first failure"))
          _              <- tracker.trackFailure(key, failure, Chunk("first"))
          failure2        = DeployFailure.classify(LicenseNotFoundException("second failure"))
          result2        <- tracker.trackFailure(key, failure2, Chunk("second"))
          state          <- ref.get
        yield
          val maybeIssue = state.issues.values.headOption
          assertTrue(
            result2.isDefined,
            state.issues.size == 1,
            maybeIssue.exists(_.comments.size == 1),
            maybeIssue.exists(_.comments.exists(_.contains("second failure"))),
          )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("Systemic failure with stale-Redis-but-existing-open-issue → adopts it instead of duplicating") {
        val key = keyFor("stale-redis")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          // Pre-populate state with an open issue matching the title we'd file.
          // Simulates: prior run filed the issue, then Redis was wiped.
          _ <- ref.update(s => s.withIssue(FakeIssue(
            number = 42,
            title = s"Deploy failed: ${key.asString}",
            body = "preexisting body",
            labels = Set("deploy-failure"),
            comments = Nil,
            closed = false,
          )))
          failure  = DeployFailure.classify(LicenseNotFoundException("after redis wipe"))
          result  <- tracker.trackFailure(key, failure, Chunk("log"))
          state   <- ref.get
          stored  <- ZIO.serviceWithZIO[Redis](_.hGet("deploy-failures", key.asString).returning[String])
        yield assertTrue(
          result.isDefined,
          state.issues.size == 1,                      // no new issue created
          state.issues.get(42).exists(_.comments.size == 1), // commented on the existing one
          stored.contains("42"),                       // Redis re-populated to the existing #
        )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("findOpenIssueByTitle is exact-match — different title → new issue") {
        val key = keyFor("title-mismatch")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          // An open issue with a different title should NOT be adopted.
          _ <- ref.update(s => s.withIssue(FakeIssue(
            number = 7,
            title = "Some other title that doesn't match",
            body = "body",
            labels = Set("deploy-failure"),
            comments = Nil,
            closed = false,
          )))
          failure = DeployFailure.classify(LicenseNotFoundException("first"))
          _ <- tracker.trackFailure(key, failure, Chunk("log"))
          state <- ref.get
        yield assertTrue(
          state.issues.size == 2,           // existing (#7) + new one we filed
          state.issues.contains(7),
          state.issues(7).comments.isEmpty, // unchanged
        )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
    ),
    suite("resolveSuccess")(
      test("no tracking entry → no-op") {
        val key = keyFor("resolve-noop")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          _              <- tracker.resolveSuccess(key)
          state          <- ref.get
        yield assertTrue(state.issues.isEmpty)
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
      test("after a Systemic failure → close + drop Redis entry on success") {
        val key = keyFor("resolve-close")
        val program = for
          (tracker, ref) <- trackerWith(Some("token"), Some(testRepo))
          failure         = DeployFailure.classify(LicenseNotFoundException("first"))
          _              <- tracker.trackFailure(key, failure, Chunk("log"))
          _              <- tracker.resolveSuccess(key)
          state          <- ref.get
          storedAfter    <- ZIO.serviceWithZIO[Redis](_.hGet("deploy-failures", key.asString).returning[String])
        yield
          val maybeIssue = state.issues.values.headOption
          assertTrue(
            state.issues.size == 1,
            maybeIssue.exists(_.closed),
            maybeIssue.exists(_.comments.exists(_.contains("Closed automatically"))),
            storedAfter.isEmpty,
          )
        program.provideLayer(TestInfrastructure.sharedRedisLayer)
      },
    ),
  ) @@ TestAspect.timeout(30.seconds)
