package webjars.utils

import com.jamesward.zio_http_guard.guard.*
import zio.*
import zio.http.*

import java.util.Locale

/**
 * webjars' configuration of zio-http-guard.
 *
 * The November 2026 flood fans each `/listfiles` URL out to ~16 proxy IPs
 * within 2 seconds, with real-browser headers and three disguises (direct
 * navigation, a google.com referer, a spoofed webjars.org referer), and
 * requests random-hex media paths that it then uses as its referer.
 *
 *   - `swarm` (on `/listfiles` paths), `probe`, `referer-probe` and a
 *     contradictory `client-hints` are Confirmed and blocked with a tiny 200:
 *     holding them open didn't slow the flood, and the cheapest answer costs
 *     us the least.
 *   - `navigation`: a browser User-Agent that didn't arrive the way a
 *     browser on webjars.org does is Suspect.
 *   - On the expensive endpoints (`/` and `/listfiles`), Regular requests
 *     run in the `regular` bulkhead and Suspect ones in the smaller
 *     `suspect` bulkhead, so suspects can't use up the capacity real users
 *     need. Everything else passes.
 *
 * Thresholds and actions can be changed with `GUARD_*` config vars (see
 * zio-http-guard's README); the defaults below are what's been running.
 */
object WebJarsGuard:

  enum Endpoint:
    case Home
    case ListFiles

  private given CanEqual[Method, Method] = CanEqual.derived

  def expensive(request: Request): Option[Endpoint] =
    if request.method != Method.GET then None
    else request.path.segments.toList match
      case Nil | List("")   => Some(Endpoint.Home)
      case "listfiles" :: _ => Some(Endpoint.ListFiles)
      case _                => None

  private val siteHosts = Set("www.webjars.org", "webjars.org")

  private def header(request: Request, name: String): Option[String] =
    request.headers.get(name).map(_.trim.toLowerCase(Locale.ROOT)).filter(_.nonEmpty)

  private def refererIsSite(request: Request): Boolean =
    request.header(Header.Referer).flatMap(_.url.host).exists(host => siteHosts.contains(host.toLowerCase(Locale.ROOT)))

  /**
   * Suspect when a `Mozilla/...` User-Agent on an expensive endpoint
   *   - for `/`, sent no `Sec-Fetch-Site` (current browsers send it on every
   *     HTTPS navigation; scripts copying a browser User-Agent often don't);
   *   - for `/listfiles`, wasn't a same-origin navigation (people reach those
   *     pages by clicking a version on webjars.org); without `Sec-Fetch-Site`
   *     a webjars.org `Referer` counts instead.
   * A visitor following an external link to a `/listfiles` page is Suspect
   * too: still served, from the smaller pool.
   */
  val navigation: Detector =
    Detector.pure(DetectorName("navigation")): (request, _) =>
      expensive(request).flatMap: endpoint =>
        val claimsBrowser = header(request, Header.UserAgent.name).exists(_.startsWith("mozilla/"))
        val secFetchSite  = header(request, "Sec-Fetch-Site")
        val arrivedLikeABrowser = endpoint match
          case Endpoint.Home      => secFetchSite.isDefined
          case Endpoint.ListFiles => secFetchSite.fold(refererIsSite(request))(_ == "same-origin")
        Option.when(claimsBrowser && !arrivedLikeABrowser)(Verdict(Tier.Suspect, Reason("navigation")))

  private def listFilesPath(request: Request): Option[String] =
    expensive(request).collect { case Endpoint.ListFiles => request.path.encode }

  private val regular = PoolName("regular")
  private val suspect = PoolName("suspect")

  private val defaults = GuardConfig(
    bulkheads = Map(regular -> GuardConfig.Pool(24, 48), suspect -> GuardConfig.Pool(4, 8)),
  )

  /** On expensive endpoints, Regular and Suspect requests run in their bulkheads. */
  private def actionFor(tier: Tier, request: Request): Option[Action] =
    expensive(request).flatMap: _ =>
      tier match
        case Tier.Regular   => Some(Action.Bulkhead(regular))
        case Tier.Suspect   => Some(Action.Bulkhead(suspect))
        case Tier.Confirmed => None

  val layer: ZLayer[Any, Config.Error, Guard] =
    ZLayer.scoped:
      for
        config <- ZIO.config(GuardConfig.config(defaults = defaults))
        swarm  <- ZIO.config(Swarm.settings()).flatMap(Swarm.make(_, listFilesPath))
        guard  <- Guard.make(
                    config,
                    Chunk(swarm, Probe.path(), Probe.referer(), ClientHints.detector, navigation),
                    actionFor,
                  )
      yield guard
