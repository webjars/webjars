package webjars.utils

import com.jamesward.zio_http_guard.BadActorMiddleware
import zio.*
import zio.concurrent.ConcurrentMap
import zio.http.*
import zio.stream.ZStream

import java.time.Instant
import java.util.Locale

/**
 * Identifies the distributed `/listfiles` flood and ties its requests up
 * instead of answering them.
 *
 * The flood fans each URL out to ~16 different IPs within about 2 seconds,
 * using real browser headers and three disguises at once (direct
 * navigation, a `www.google.com` referer, a spoofed `www.webjars.org`
 * referer). Headers can't separate it from real visitors, so the signals
 * here are behavioural:
 *
 *   - [[Reason.Swarm]]: the same `/listfiles` URL was requested by
 *     [[swarmIps]] or more distinct IPs within [[swarmWindow]].
 *   - [[Reason.Probe]]: a root-level random-hex media path such as
 *     `/947870aad1.mkv`, which the flood requests before navigating to a
 *     `/listfiles` page.
 *   - [[Reason.ProbeReferer]]: the `Referer` is such a probe path.
 *   - [[Reason.Flagged]]: the IP was identified by one of the above within
 *     [[flagFor]].
 *
 * Identified requests get a [[tarpit]] response: a `200` that trickles a
 * byte every few seconds and never completes a page, so a browser worker
 * in the flood sits on it until its own navigation timeout. At most
 * [[maxTarpits]] are held at once; past that they get a plain `503`.
 */
case class BadActors(
  urlHits: ConcurrentMap[String, Chunk[(Instant, String)]],
  flagged: ConcurrentMap[String, Instant],
  tarpits: Ref[Int],
):
  import BadActors.*

  private def recordHit(url: String, ip: String, now: Instant): UIO[Int] =
    urlHits
      .compute(url, (_, existing) => Some(existing.getOrElse(Chunk.empty).filter((at, _) => isRecent(at, now, swarmWindow)) :+ (now -> ip)))
      .map(_.fold(0)(_.map(_._2).distinct.size))

  private def isFlagged(ip: String, now: Instant): UIO[Boolean] =
    flagged.get(ip).map(_.exists(isRecent(_, now, flagFor)))

  def identify(endpoint: LoadShedding.Endpoint | Probe.type, ip: String, request: Request): UIO[Option[Reason]] =
    for
      now          <- Clock.instant
      wasFlagged   <- isFlagged(ip, now)
      distinctIps  <- endpoint match
                        case LoadShedding.Endpoint.ListFiles => recordHit(request.path.encode, ip, now)
                        case _                               => ZIO.succeed(0)
      reason        = endpoint match
                        case Probe                                            => Some(Reason.Probe)
                        case _ if refererIsProbe(request)                     => Some(Reason.ProbeReferer)
                        case _ if distinctIps >= swarmIps                     => Some(Reason.Swarm)
                        case _ if wasFlagged                                  => Some(Reason.Flagged)
                        case _                                                => None
      _            <- flagged.put(ip, now).when(reason.exists(_ != Reason.Flagged))
    yield reason

  /** A trickling response if a tarpit slot is free, otherwise a cheap 503. */
  val tarpit: UIO[Response] =
    tarpits.modify(n => if n < maxTarpits then (true, n + 1) else (false, n)).map:
      case true =>
        Response(
          Status.Ok,
          Headers(Header.ContentType(MediaType.text.html), Header.CacheControl.NoStore),
          Body.fromStreamChunked(trickle.ensuring(tarpits.update(_ - 1))),
        )
      case false =>
        LoadShedding.rejected

  private val sweep: UIO[Unit] =
    for
      now <- Clock.instant
      _   <- urlHits.removeIf((_, hits) => hits.forall((at, _) => !isRecent(at, now, swarmWindow)))
      _   <- flagged.removeIf((_, at) => !isRecent(at, now, flagFor))
    yield ()

  private val report: UIO[Unit] =
    for
      held       <- tarpits.get
      flaggedIps <- flagged.toChunk.map(_.size)
      urls       <- urlHits.toChunk.map(_.size)
      _          <- ZIO.logInfo(s"bad_actors tarpits_held=$held flagged_ips=$flaggedIps tracked_urls=$urls")
    yield ()

  /**
   * Tarpits identified requests and adds a `traffic_class` log annotation
   * to guarded ones. Apply outside the request logger so its lines carry
   * the annotation. Tarpitted requests never reach the logger, so they get
   * their own `bad_actor` line.
   */
  val screen: Middleware[Any] =
    new Middleware[Any]:
      def apply[Env1, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
        routes.transform[Env1]: handler =>
          Handler.scoped[Env1]:
            Handler.fromFunctionZIO[Request]: request =>
              val guarded = LoadShedding.guarded(request)
              val target: Option[LoadShedding.Endpoint | Probe.type] =
                if isProbe(request.path) then Some(Probe) else guarded
              val served = guarded.fold(handler(request)): endpoint =>
                ZIO.logAnnotate("traffic_class", LoadShedding.classify(endpoint, request).toString)(handler(request))
              (target, BadActorMiddleware.forwardedFor(request)) match
                case (Some(endpoint), Some(ip)) =>
                  identify(endpoint, ip, request).flatMap:
                    case Some(reason) =>
                      val ua      = request.headers.get(Header.UserAgent.name).getOrElse("-")
                      val referer = request.headers.get(Header.Referer.name).getOrElse("-")
                      val site    = request.headers.get("Sec-Fetch-Site").getOrElse("-")
                      ZIO.logInfo(s"bad_actor reason=$reason ip=$ip url=${request.path.encode} sec-fetch-site=$site referer=$referer user-agent=$ua") *> tarpit
                    case None =>
                      served
                case _ =>
                  served

object BadActors:

  enum Reason:
    case Swarm
    case Probe
    case ProbeReferer
    case Flagged

  given CanEqual[Reason, Reason] = CanEqual.derived

  /** Marker for a root-level random-hex media path. */
  case object Probe

  val swarmWindow: Duration = 5.seconds
  val swarmIps: Int = 4
  val flagFor: Duration = 10.minutes
  val maxTarpits: Int = 1000
  val tarpitFor: Duration = 2.minutes
  val trickleEvery: Duration = 5.seconds

  private def isRecent(at: Instant, now: Instant, window: Duration): Boolean =
    java.time.Duration.between(at, now).compareTo(window.asJava) < 0

  private val probeName = "^[0-9a-f]{8,}\\.(avi|flac|flv|mka|mkv|mov|mp3|mp4|mpeg|mpg|ogg|webm|webp|wmv)$".r

  def isProbe(path: Path): Boolean =
    path.segments.toList match
      case List(segment) => probeName.matches(segment.toLowerCase(Locale.ROOT))
      case _             => false

  private def refererIsProbe(request: Request): Boolean =
    request.header(Header.Referer).exists(referer => isProbe(referer.url.path))

  /**
   * The start of an HTML page, then a space every [[trickleEvery]] until
   * [[tarpitFor]] passes. Heroku's router needs the first byte within 30
   * seconds and then any byte within each 55 seconds, which this keeps.
   */
  private val trickle: ZStream[Any, Nothing, Byte] =
    (ZStream.succeed(Chunk.fromArray("<!DOCTYPE html><html><head>".getBytes)) ++
      ZStream.repeatWithSchedule(Chunk.single(' '.toByte), Schedule.spaced(trickleEvery)))
      .flattenChunks
      .interruptAfter(tarpitFor)

  val layer: ZLayer[Any, Nothing, BadActors] =
    ZLayer.scoped:
      for
        urlHits   <- ConcurrentMap.empty[String, Chunk[(Instant, String)]]
        flagged   <- ConcurrentMap.empty[String, Instant]
        tarpits   <- Ref.make(0)
        badActors  = BadActors(urlHits, flagged, tarpits)
        _         <- (badActors.sweep *> badActors.report).repeat(Schedule.spaced(30.seconds)).forkScoped
      yield badActors
