package webjars.utils

import com.jamesward.zio_http_guard.BadActorMiddleware
import zio.*
import zio.concurrent.ConcurrentMap
import zio.http.*

/**
 * Per-minute traffic summary for spotting floods: total requests, distinct
 * client IPs, the top IPs and `/16` networks by request count, and the top
 * paths by number of distinct IPs.
 *
 * A single heavy hitter shows up in `top_ips`. A swarm spread over many IPs
 * shows up as a high `distinct_ips` with every IP near 1, and as paths in
 * `top_swarmed_paths` requested by many different IPs (the `/listfiles`
 * flood fans each URL out to ~16).
 */
final case class TrafficStats(
  byIp: ConcurrentMap[String, Long],
  ipsByPath: ConcurrentMap[String, Set[String]],
):
  import TrafficStats.*

  def record(ip: String, path: String): UIO[Unit] =
    byIp.compute(ip, (_, n) => Some(n.getOrElse(0L) + 1)) *>
      ipsByPath.compute(path, (_, ips) => Some(ips.getOrElse(Set.empty) + ip)).unit

  /** Counts every request by client IP and path. Apply outermost. */
  val middleware: Middleware[Any] =
    new Middleware[Any]:
      def apply[Env1, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
        routes.transform[Env1]: handler =>
          Handler.scoped[Env1]:
            Handler.fromFunctionZIO[Request]: request =>
              ZIO.foreachDiscard(BadActorMiddleware.forwardedFor(request))(record(_, request.path.encode)) *>
                handler(request)

  private def drain[V](map: ConcurrentMap[String, V]): UIO[Chunk[(String, V)]] =
    map.toChunk.flatMap(entries => ZIO.foreachDiscard(entries)((key, _) => map.remove(key)).as(entries))

  private def top(counts: Iterable[(String, Long)]): String =
    counts.toSeq.sortBy(-_._2).take(topN).map((key, n) => s"$key=$n").mkString(",")

  private val report: UIO[Unit] =
    for
      requests <- drain(byIp)
      paths    <- drain(ipsByPath)
      networks  = requests.groupMapReduce((ip, _) => network(ip))(_._2)(_ + _)
      swarmed   = paths.map((path, ips) => path -> ips.size.toLong).filter(_._2 > 1)
      _        <- ZIO.logInfo(
                    s"traffic_stats requests=${requests.map(_._2).sum} distinct_ips=${requests.size} " +
                    s"distinct_paths=${paths.size} top_ips=${top(requests)} top_networks=${top(networks)} " +
                    s"top_swarmed_paths=${top(swarmed)}"
                  )
    yield ()

object TrafficStats:

  val interval: Duration = 1.minute
  private val topN = 10

  /** The `/16` of an IPv4 address, or the address itself otherwise. */
  def network(ip: String): String =
    ip.split('.') match
      case Array(a, b, _, _) => s"$a.$b.0.0/16"
      case _                 => ip

  val layer: ZLayer[Any, Nothing, TrafficStats] =
    ZLayer.scoped:
      for
        byIp      <- ConcurrentMap.empty[String, Long]
        ipsByPath <- ConcurrentMap.empty[String, Set[String]]
        stats      = TrafficStats(byIp, ipsByPath)
        _         <- stats.report.repeat(Schedule.spaced(interval)).delay(interval).forkScoped
      yield stats
