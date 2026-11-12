package webjars.utils

import nl.vroste.rezilience.Bulkhead
import zio.*
import zio.http.*

import java.util.Locale

/**
 * Concurrency limits for the endpoints a request flood can make expensive:
 * `/listfiles/...` (file-service call, file-list render) and `/` (a ~240 KB
 * page rendered per request).
 *
 * Requests on those endpoints are split into two traffic classes, each with
 * its own rezilience [[Bulkhead]]:
 *
 *   - [[TrafficClass.Suspect]]: claims to be a browser but didn't arrive the
 *     way a browser on webjars.org does (see [[classify]]). These share a
 *     small bulkhead, so a flood of them can't use up the capacity regular
 *     users need.
 *   - [[TrafficClass.Regular]]: everything else, including API clients that
 *     identify themselves honestly (curl, build tools, other services).
 *
 * When a class's bulkhead and its queue are full, the request gets an
 * immediate `503` with `Retry-After` instead of queueing without bound.
 * Other endpoints pass through untouched.
 */
case class LoadShedding(regular: Bulkhead, suspect: Bulkhead, rejections: Ref[Map[LoadShedding.TrafficClass, Long]]):
  import LoadShedding.*

  /** Runs guarded requests through their class's bulkhead. Apply inside the request logger so 503s get logged. */
  val limit: Middleware[Any] =
    new Middleware[Any]:
      def apply[Env1, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
        routes.transform[Env1]: handler =>
          Handler.scoped[Env1]:
            Handler.fromFunctionZIO[Request]: request =>
              guarded(request) match
                case None =>
                  handler(request)
                case Some(endpoint) =>
                  val trafficClass = classify(endpoint, request)
                  val bulkhead = trafficClass match
                    case TrafficClass.Regular => regular
                    case TrafficClass.Suspect => suspect
                  bulkhead(handler(request)).catchAll:
                    _.fold(
                      rejections.update(counts => counts.updated(trafficClass, counts.getOrElse(trafficClass, 0L) + 1)) *> ZIO.fail(rejected),
                      ZIO.fail(_),
                    )

  /**
   * Adds a `traffic_class` log annotation to guarded requests. Apply outside
   * the request logger so its "Http request served" line carries the
   * classification, which is how we check [[classify]] against real traffic.
   */
  val annotate: Middleware[Any] =
    new Middleware[Any]:
      def apply[Env1, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
        routes.transform[Env1]: handler =>
          Handler.scoped[Env1]:
            Handler.fromFunctionZIO[Request]: request =>
              guarded(request) match
                case None =>
                  handler(request)
                case Some(endpoint) =>
                  ZIO.logAnnotate("traffic_class", classify(endpoint, request).toString)(handler(request))

  /** Bulkhead occupancy and rejection counts since the previous report. */
  val report: UIO[Unit] =
    for
      regularMetrics <- regular.metrics
      suspectMetrics <- suspect.metrics
      counts         <- rejections.getAndSet(Map.empty)
      runtime         = java.lang.Runtime.getRuntime.nn
      heapUsedMb      = (runtime.totalMemory - runtime.freeMemory) / 1048576
      _              <- ZIO.logInfo(
                          s"load_shedding heap_used_mb=$heapUsedMb " +
                          s"regular_in_flight=${regularMetrics.inFlight} regular_queued=${regularMetrics.inQueue} " +
                          s"regular_rejected=${counts.getOrElse(TrafficClass.Regular, 0L)} " +
                          s"suspect_in_flight=${suspectMetrics.inFlight} suspect_queued=${suspectMetrics.inQueue} " +
                          s"suspect_rejected=${counts.getOrElse(TrafficClass.Suspect, 0L)}"
                        )
    yield ()

object LoadShedding:

  enum Endpoint:
    case Home
    case ListFiles

  enum TrafficClass:
    case Regular
    case Suspect

  given CanEqual[TrafficClass, TrafficClass] = CanEqual.derived

  private given CanEqual[Method, Method] = CanEqual.derived

  def guarded(request: Request): Option[Endpoint] =
    if request.method != Method.GET then None
    else request.path.segments.toList match
      case Nil | List("")   => Some(Endpoint.Home)
      case "listfiles" :: _ => Some(Endpoint.ListFiles)
      case _                => None

  private def header(request: Request, name: String): Option[String] =
    request.headers.get(name).map(_.trim.toLowerCase(Locale.ROOT)).filter(_.nonEmpty)

  private val siteHosts = Set("www.webjars.org", "webjars.org")

  private def refererIsSite(request: Request): Boolean =
    request.header(Header.Referer).flatMap(_.url.host).exists(host => siteHosts.contains(host.toLowerCase(Locale.ROOT)))

  /**
   * A request is [[TrafficClass.Suspect]] when its User-Agent claims to be a
   * browser (`Mozilla/...`) and:
   *
   *   - for `/`: it sent no `Sec-Fetch-Site` header. Current Chrome, Edge,
   *     Firefox, and Safari (16.4+) send it on every HTTPS navigation;
   *     scripts that copy a browser User-Agent usually don't.
   *   - for `/listfiles`: it wasn't a same-origin navigation. Users reach
   *     these pages by clicking a version on webjars.org. When the browser
   *     sends no `Sec-Fetch-Site`, a webjars.org `Referer` counts instead.
   *
   * A real visitor following an external link straight to a `/listfiles`
   * page lands in the suspect class. They're still served, just from the
   * smaller pool.
   */
  def classify(endpoint: Endpoint, request: Request): TrafficClass =
    val claimsBrowser = header(request, Header.UserAgent.name).exists(_.startsWith("mozilla/"))
    val secFetchSite = header(request, "Sec-Fetch-Site")
    val arrivedLikeABrowser = endpoint match
      case Endpoint.Home      => secFetchSite.isDefined
      case Endpoint.ListFiles => secFetchSite.fold(refererIsSite(request))(_ == "same-origin")
    if claimsBrowser && !arrivedLikeABrowser then TrafficClass.Suspect else TrafficClass.Regular

  val retryAfter: Duration = 30.seconds

  val reportEvery: Duration = 30.seconds

  val rejected: Response =
    Response(
      Status.ServiceUnavailable,
      Headers(Header.RetryAfter.ByDuration(retryAfter)),
      Body.fromString("Too busy, try again shortly"),
    )

  val layer: ZLayer[Any, Nothing, LoadShedding] =
    ZLayer.scoped:
      for
        regular      <- Bulkhead.make(maxInFlightCalls = 24, maxQueueing = 48)
        suspect      <- Bulkhead.make(maxInFlightCalls = 4, maxQueueing = 8)
        rejections   <- Ref.make(Map.empty[TrafficClass, Long])
        loadShedding  = LoadShedding(regular, suspect, rejections)
        _            <- loadShedding.report.repeat(Schedule.fixed(reportEvery)).delay(reportEvery).forkScoped
      yield loadShedding
