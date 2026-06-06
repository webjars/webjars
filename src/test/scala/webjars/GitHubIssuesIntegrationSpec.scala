package webjars

import webjars.utils.*
import zio.*
import zio.direct.*
import zio.http.Client
import zio.test.*

/** Live integration test for the GitHub-issues protocol layer.
 *
 *  Self-skips unless both `GITHUB_TOKEN` and `WEBJARS_TEST_REPO` are set in
 *  the environment. Production code never reads `WEBJARS_TEST_REPO` — it's
 *  test-only — so this spec is the only place that env var is consulted.
 *
 *  What this catches that the unit-level [[DeployFailureTrackerSpec]]
 *  doesn't:
 *
 *    - endpoint paths and HTTP methods (PATCH for close, POST for create
 *      and comments)
 *    - JSON field names (`number`, `html_url`)
 *    - status-code expectations (`Created` for POST, `Ok` for PATCH)
 *    - URL-encoding of the label query parameter
 *    - auth header format
 *
 *  Strategy: a single round-trip test that exercises every method of the
 *  trait against a sandbox repo. Issue title carries a UUID so concurrent
 *  runs don't collide. Issue is closed in `acquireRelease` so even on
 *  failure or interrupt we don't leave orphan issues in the test repo.
 *
 *  Note on consistency: GitHub's `/repos/.../issues?state=open&labels=…`
 *  listing endpoint is eventually consistent with respect to recent
 *  creates and closes — it can take a few seconds for a fresh issue to
 *  appear in the listing or for a closed one to drop off. The
 *  [[findOpenIssueByTitle]] assertions poll with a short backoff to
 *  smooth over that lag. The `createIssue`/`closeIssue` calls themselves
 *  are strongly consistent — their success is verified by the call
 *  result, not by re-reading via the listing. */
object GitHubIssuesIntegrationSpec extends ZIOSpecDefault:

  /** Build a real `GitHubLive` against the live API. */
  private def liveGitHub: ZIO[Client, Nothing, GitHub] =
    ZIO.serviceWith[Client] { client =>
      val token = sys.env.get("GITHUB_TOKEN")
      val cfg = TestInfrastructure.testConfig.copy(githubAuthToken = token)
      GitHubLive(client, cfg, CacheLive())
    }

  private def requireRepo: ZIO[Any, AssertionError, GitHub.Repo] =
    ZIO.fromOption(sys.env.get("WEBJARS_TEST_REPO").flatMap(GitHub.Repo.parse))
      .orElseFail(new AssertionError(
        "WEBJARS_TEST_REPO must be set to <owner>/<repo>. The integration spec self-skips when unset, " +
          "so seeing this AssertionError means TestAspect.ifEnvSet was bypassed."
      ))

  /** Distinct title per run so concurrent CI invocations don't collide.
   *  We also include the label name in the title so a human looking at
   *  the test repo can tell what this is from. */
  private def uniqueTitle: UIO[String] =
    Random.nextUUID.map(u => s"webjars integration-test issue $u")

  /** Poll `findOpenIssueByTitle` until the predicate holds (typically
   *  "Some" right after create; "None" right after close). Caps at
   *  ~30 seconds with exponential backoff — on GitHub's prod API, both
   *  states usually settle within 1-3s but a stuck CI run shouldn't
   *  hang forever. */
  private def pollUntil(
    gh: GitHub,
    repo: GitHub.Repo,
    title: String,
    label: String,
  )(predicate: Option[GitHub.Issue] => Boolean): ZIO[Scope, Throwable, Option[GitHub.Issue]] =
    val schedule = Schedule.recurs(8) && Schedule.exponential(250.millis).whileOutput(_ < 5.seconds)
    gh.findOpenIssueByTitle(repo, title, label)
      .repeat(Schedule.recurUntil[Option[GitHub.Issue]](predicate) && schedule)
      .map(_._1)

  def spec = suite("GitHubIssuesIntegrationSpec")(
    test("round-trip: create → find → comment → close → confirm-closed") {
      ZIO.scoped[Client] {
        defer:
          val gh = liveGitHub.run
          val repo = requireRepo.run
          val title = uniqueTitle.run

          // Acquire = file the issue. Release = make sure it ends up
          // closed regardless of whether the test body succeeds, fails,
          // or is interrupted. We close best-effort (`.ignoreLogged`) so
          // a transient GitHub blip during cleanup doesn't mask the
          // actual test result.
          val issue = ZIO.acquireRelease(
            gh.createIssue(repo, title, "integration-test body — safe to close at any time", Set("deploy-failure"))
          ) { issue =>
            gh.closeIssue(repo, issue.number, comment = Some("integration-test cleanup"))
              .ignoreLogged
          }.run

          // After create: the issue should eventually show up in the
          // open-issues listing. Poll past GitHub's listing-propagation
          // lag (~1-3s in practice).
          val foundOpen = pollUntil(gh, repo, title, "deploy-failure")(_.exists(_.number == issue.number)).run

          // Comment on it.
          gh.commentOnIssue(repo, issue.number, "integration-test recurring-failure comment").run

          // Close it explicitly (this is what the test is really
          // verifying — release-on-cleanup will then be idempotent).
          gh.closeIssue(repo, issue.number, comment = Some("integration-test resolution comment")).run

          // After close: should eventually drop out of the open-issues
          // listing. Same propagation-lag tolerance.
          val foundAfterClose = pollUntil(gh, repo, title, "deploy-failure")(_.isEmpty).run

          assertTrue(
            issue.number > 0,
            issue.htmlUrl.encode.contains(s"/${repo.slug}/issues/${issue.number}"),
            foundOpen.exists(_.number == issue.number),
            foundOpen.exists(_.htmlUrl.encode.contains(s"/${repo.slug}/issues/${issue.number}")),
            foundAfterClose.isEmpty,
          )
      }
    } @@ TestAspect.timeout(2.minutes),
  ).provide(Client.default) @@
    TestAspect.withLiveClock @@
    TestAspect.withLiveRandom @@
    TestAspect.ifEnvSet("GITHUB_TOKEN") @@
    TestAspect.ifEnvSet("WEBJARS_TEST_REPO")
