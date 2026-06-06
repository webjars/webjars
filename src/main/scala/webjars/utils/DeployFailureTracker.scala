package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import zio.*
import zio.direct.*
import zio.http.URL
import zio.redis.*

import scala.util.matching.Regex

/** Tracks `Systemic` deploy failures by filing or updating a single GitHub
 *  issue per `(groupId, nameOrUrlish, upstreamVersion)`. Idempotent —
 *  repeated failures of the same key add comments to one tracking issue
 *  rather than fanning out into duplicates. Safe to call on `UserInput`
 *  and `Transient` failures: they short-circuit to `None`.
 *
 *  Forward index lives in Redis hash `deploy-failures`:
 *  field = `<groupId>:<nameOrUrlish>:<version>`, value = issue number.
 *  When the index has no entry, we still scan open issues by exact
 *  title match (deterministic format) and adopt the existing one if
 *  found — covers the case where Redis was wiped or the prior write
 *  raced.
 *
 *  All errors are logged and swallowed: a flaky GitHub API must never
 *  abort an otherwise-successful deploy.
 *
 *  Auth: requires `webjars.github.auth-token` (env `GITHUB_TOKEN`) with
 *  `Issues: read & write` on the configured tracking repo. If absent,
 *  every track call short-circuits to `None` (no spam, no errors). */
trait DeployFailureTracker:
  def trackFailure(
    deployKey: DeployFailureTracker.DeployKey,
    failure: DeployFailure,
    deployLog: Chunk[String],
  ): URIO[Redis, Option[URL]]

  /** On a successful deploy of `deployKey`, close any tracking issue we
   *  filed for it. No-op when there's no tracking entry. */
  def resolveSuccess(deployKey: DeployFailureTracker.DeployKey): URIO[Redis, Unit]

object DeployFailureTracker:

  /** The dedup key for a deploy attempt — same shape as a Maven Central
   *  GAV, except we use `nameOrUrlish` (what the user typed) instead of
   *  the resolved artifactId because resolution itself can fail before
   *  we know the artifactId. */
  case class DeployKey(groupId: MavenCentral.GroupId, nameOrUrlish: String, version: String):
    def asString: String = s"$groupId:$nameOrUrlish:$version"

  // Redis hash key. Single hash so admins can `HGETALL deploy-failures`
  // to see what's currently being tracked.
  private[utils] val redisHashKey: String = "deploy-failures"

  // Label applied to every issue we file. Filtering on this label keeps
  // us from accidentally adopting unrelated open issues that happen to
  // match the title pattern.
  private[utils] val issueLabel: String = "deploy-failure"

  /** Deterministic title — must be stable across calls so the
   *  search-by-title fallback can find existing issues. */
  private[utils] def issueTitle(deployKey: DeployKey): String =
    s"Deploy failed: ${deployKey.asString}"

  /** Body of a fresh issue. Contains the failure category, the message,
   *  and the deploy log so a maintainer has enough context to reproduce.
   *  All free-text fields run through [[scrub]] before being included. */
  private[utils] def initialIssueBody(deployKey: DeployKey, failure: DeployFailure, deployLog: Chunk[String]): String =
    val tag = DeployFailure.tag(failure)
    val message = scrub(failure.message)
    val log = scrub(deployLog.mkString("\n")).take(maxLogChars)
    s"""**Deploy key:** `${deployKey.asString}`
       |**Category:** $tag
       |**Error:** ${codeBlock(message)}
       |
       |### Deploy log
       |
       |```
       |$log
       |```
       |
       |---
       |_Filed automatically by webjars.org. The issue will be closed when
       |a subsequent deploy of `${deployKey.asString}` succeeds._
       |""".stripMargin

  /** Body of a comment when the same deploy key fails again. Just the
   *  category + message + tail of the log — we don't repeat the full
   *  preamble. */
  private[utils] def recurringFailureCommentBody(failure: DeployFailure, deployLog: Chunk[String]): String =
    val tag = DeployFailure.tag(failure)
    val message = scrub(failure.message)
    val log = scrub(deployLog.takeRight(40).mkString("\n")).take(maxLogChars)
    s"""Reoccurred at ${java.time.Instant.now()} — category: $tag.
       |
       |**Error:** ${codeBlock(message)}
       |
       |### Recent deploy log (tail)
       |
       |```
       |$log
       |```
       |""".stripMargin

  /** Body of the comment posted when a successful deploy resolves the
   *  failure. */
  private[utils] def resolutionCommentBody(deployKey: DeployKey): String =
    s"Closed automatically — `${deployKey.asString}` deployed successfully at ${java.time.Instant.now()}."

  /** Cap on log content per body — keeps us under GitHub's ~65k character
   *  limit on issue/comment bodies even when the deploy log is huge. */
  private val maxLogChars: Int = 30_000

  /** Wrap inline code so a long single-line message doesn't blow out
   *  the layout. Fenced if it contains a backtick already. */
  private def codeBlock(s: String): String =
    if s.contains("\n") || s.length > 120 then s"\n```\n$s\n```"
    else s"`$s`"

  /** Patterns we redact from any text we'd post publicly. Each entry is
   *  (regex, replacement) — the replacement may use `$1`, `$2`, … to
   *  refer back to capture groups. We apply patterns in order so a
   *  more-specific token-shape pattern can win over the userinfo one
   *  if it would otherwise overlap.
   *
   *  - GitHub tokens (classic & fine-grained): `gh[opsur]_` prefix.
   *  - Personal-access-token format: `github_pat_…`.
   *  - URL userinfo (`https://user:pw@host`).
   *  - Generic Authorization header values (`Bearer <token>`,
   *    `token <hex>`).
   *
   *  Add aggressively, never loosen — the cost of a false positive
   *  (some innocuous string redacted) is far lower than a real token
   *  ending up on a public issue. */
  private val redactionPatterns: Seq[(Regex, String)] = Seq(
    // GitHub tokens of any flavor.
    """gh[opsur]_[A-Za-z0-9_]{20,}""".r            -> "gh*_REDACTED",
    """github_pat_[A-Za-z0-9_]{20,}""".r           -> "github_pat_REDACTED",
    // userinfo in URLs (catches `https://user:pw@host` and `https://token@host`).
    """([a-zA-Z][a-zA-Z0-9+.\-]*://)[^\s/@]+@""".r -> "$1REDACTED@",
    // `Authorization: Bearer …` headers / `Bearer …` standalone.
    """(?i)Bearer\s+[A-Za-z0-9._\-]+""".r          -> "Bearer REDACTED",
    """(?i)token\s+[A-Fa-f0-9]{20,}""".r           -> "token REDACTED",
  )

  /** Run all redaction patterns over the input string. The replacement
   *  strings may contain `$1`-style backreferences and we DO NOT
   *  `quoteReplacement` them — we control these patterns at compile
   *  time and need the backrefs to fire. */
  def scrub(s: String): String =
    redactionPatterns.foldLeft(s) { case (acc, (pattern, replacement)) =>
      pattern.replaceAllIn(acc, replacement)
    }

  /** Lookup the tracking issue number for a deploy key, if any. */
  private[utils] def lookupIssueNumber(deployKey: DeployKey): ZIO[Redis, RedisError, Option[Int]] =
    ZIO.serviceWithZIO[Redis] { redis =>
      redis.hGet(redisHashKey, deployKey.asString).returning[String]
        .map(_.flatMap(_.toIntOption))
    }

  private[utils] def storeIssueNumber(deployKey: DeployKey, issueNumber: Int): ZIO[Redis, RedisError, Unit] =
    ZIO.serviceWithZIO[Redis] { redis =>
      redis.hSet(redisHashKey, deployKey.asString -> issueNumber.toString).unit
    }

  private[utils] def forgetIssueNumber(deployKey: DeployKey): ZIO[Redis, RedisError, Unit] =
    ZIO.serviceWithZIO[Redis] { redis =>
      redis.hDel(redisHashKey, deployKey.asString).unit
    }

  case class DeployFailureTrackerLive(gitHub: GitHub, config: AppConfig) extends DeployFailureTracker:

    private val maybeRepo: Option[GitHub.Repo] =
      config.deployFailureIssuesRepo.flatMap(GitHub.Repo.parse)

    /** Fast path that decides whether to bother. */
    private def shouldRun(failure: DeployFailure): Boolean =
      DeployFailure.shouldFileIssue(failure) &&
        gitHub.maybeAuthToken.isDefined &&
        maybeRepo.isDefined

    def trackFailure(
      deployKey: DeployKey,
      failure: DeployFailure,
      deployLog: Chunk[String],
    ): URIO[Redis, Option[URL]] =
      if !shouldRun(failure) then ZIO.none
      else
        val repo = maybeRepo.get
        val title = issueTitle(deployKey)

        val program: ZIO[Scope & Redis, Throwable, URL] =
          defer:
            val maybeNum = lookupIssueNumber(deployKey).run
            maybeNum match
              case Some(existingNum) =>
                gitHub.commentOnIssue(repo, existingNum, recurringFailureCommentBody(failure, deployLog)).run
                URL.unsafeParse(s"https://github.com/${repo.slug}/issues/$existingNum")
              case None =>
                gitHub.findOpenIssueByTitle(repo, title, issueLabel).run match
                  case Some(found) =>
                    storeIssueNumber(deployKey, found.number).run
                    gitHub.commentOnIssue(repo, found.number, recurringFailureCommentBody(failure, deployLog)).run
                    found.htmlUrl
                  case None =>
                    val issue = gitHub.createIssue(
                      repo,
                      title,
                      initialIssueBody(deployKey, failure, deployLog),
                      Set(issueLabel),
                    ).run
                    storeIssueNumber(deployKey, issue.number).run
                    issue.htmlUrl

        ZIO.scoped(program)
          .map(Some(_))
          .catchAllCause { cause =>
            ZIO.logWarningCause(s"deploy-failure tracker: failed to file/update issue for ${deployKey.asString}", cause).as(None)
          }

    def resolveSuccess(deployKey: DeployKey): URIO[Redis, Unit] =
      if maybeRepo.isEmpty || gitHub.maybeAuthToken.isEmpty then ZIO.unit
      else
        val repo = maybeRepo.get
        val program: ZIO[Scope & Redis, Throwable, Unit] =
          defer:
            val maybeNum = lookupIssueNumber(deployKey).run
            maybeNum match
              case Some(existingNum) =>
                gitHub.closeIssue(repo, existingNum, Some(resolutionCommentBody(deployKey))).run
                forgetIssueNumber(deployKey).run
              case None =>
                ()
        ZIO.scoped(program)
          .catchAllCause { cause =>
            ZIO.logWarningCause(s"deploy-failure tracker: failed to auto-close issue for ${deployKey.asString}", cause)
          }

  val live: ZLayer[GitHub & AppConfig, Nothing, DeployFailureTracker] =
    ZLayer.derive[DeployFailureTrackerLive]
