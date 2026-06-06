package webjars.utils

import java.io.FileNotFoundException
import java.net.{ConnectException, SocketTimeoutException, UnknownHostException}
import java.util.concurrent.TimeoutException

/** Classification for a `Throwable` that escaped the deploy pipeline.
 *
 *  Three buckets:
 *
 *  - [[DeployFailure.UserInput]] — caused by something the deployer
 *    (the human pressing "Deploy") needs to fix. The package they asked for
 *    doesn't exist, the version is wrong, the artifact is already on Maven
 *    Central, etc. We surface the message but DO NOT file an issue.
 *  - [[DeployFailure.Transient]] — a downstream service flaked or rate-
 *    limited us. Likely fixes itself on retry. We surface the message but
 *    DO NOT file an issue (they'd just accumulate as ghost noise).
 *  - [[DeployFailure.Systemic]] — a real problem in our code or in the
 *    package's published metadata. File an issue so a maintainer sees it
 *    and can either patch the code path or open an upstream PR.
 *
 *  Default for unrecognized `Throwable`s is `Transient`: the goal of the
 *  issue tracker is to highlight real bugs, not to mirror every error log
 *  the deploy could ever produce. */
sealed trait DeployFailure:
  val cause: Throwable
  val message: String = Option(cause.getMessage).filter(_.nonEmpty).getOrElse(cause.getClass.getSimpleName)

object DeployFailure:

  case class UserInput(cause: Throwable) extends DeployFailure
  case class Transient(cause: Throwable) extends DeployFailure
  case class Systemic(cause: Throwable) extends DeployFailure

  /** Is this failure worth filing as a GitHub issue?
   *  Only `Systemic` failures are. */
  def shouldFileIssue(failure: DeployFailure): Boolean = failure match
    case _: Systemic => true
    case _           => false

  /** Map a `Throwable` thrown during deploy into one of the three buckets.
   *  Order matters: more-specific cases come first.
   *
   *  When extending: prefer adding a specific sealed-exception case at the
   *  top of the match. Resist the urge to bucket new generic `Exception`s
   *  as `Systemic` "just in case" — that's how the issue tracker fills up
   *  with retryable network burps. */
  def classify(t: Throwable): DeployFailure = t match
    // Domain failures defined in this codebase.
    case _: LicenseNotFoundException             => Systemic(t)
    case _: NoValidLicenses                      => Systemic(t)
    case _: MissingMetadataException             => Systemic(t)
    case _: UnauthorizedError                    => Systemic(t)

    // ServerError: classify by HTTP status. 404 is "you asked for a thing
    // that doesn't exist" — that's the deployer's input. 401/403 is "we
    // can't auth to upstream" — config problem on our side. 429/5xx is the
    // upstream provider hiccuping.
    case ServerError(_, status) if status == 404                    => UserInput(t)
    case ServerError(_, status) if status == 401 || status == 403   => Systemic(t)
    case ServerError(_, status) if status == 429                    => Transient(t)
    case ServerError(_, status) if status >= 500 && status < 600    => Transient(t)
    case _: ServerError                                             => Transient(t)

    // The only place `IllegalStateException` is thrown today is the
    // already-deployed pre-check — that's a happy "your version's already
    // on Maven Central" rerouted as a failure. Strictly UserInput.
    case _: IllegalStateException                                   => UserInput(t)

    // A missing file in the upstream archive (no `package.json`, missing
    // build artifacts, etc.) is a real packaging problem we'd want to
    // know about, not something the deployer can fix without filing
    // upstream.
    case _: FileNotFoundException                                   => Systemic(t)

    // Anything that looks network-ish.
    case _: TimeoutException                                        => Transient(t)
    case _: SocketTimeoutException                                  => Transient(t)
    case _: ConnectException                                        => Transient(t)
    case _: UnknownHostException                                    => Transient(t)

    // Everything else: be conservative. We'd rather miss an issue than
    // spam the tracker with unknown junk.
    case _                                                          => Transient(t)

  /** Short tag used in the SSE log line so the user (and the test) can
   *  tell which bucket a failure landed in. */
  def tag(failure: DeployFailure): String = failure match
    case _: UserInput => "user-input"
    case _: Transient => "transient"
    case _: Systemic  => "systemic"
