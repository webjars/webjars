package webjars.utils

import com.jamesward.zio_http_guard.CrawlerLimiter
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.{ArtifactId, GroupArtifactVersion, GroupId, Version}
import zio.*
import zio.http.*

import java.net.URLDecoder

/**
 * Wiring for `zio-http-guard`'s [[CrawlerLimiter]] tailored to the webjars
 * service.
 *
 * The expensive endpoints are `/listfiles/...` — they fetch + extract a jar
 * from Maven Central. Crawlers (Googlebot, GPTBot, etc.) walking the WebJars
 * site can otherwise trigger many concurrent extractions for unrelated GAVs.
 *
 * The limiter holds a single GAV slot per crawler User-Agent. While a slot
 * is held, requests for the **same** GAV pass (the cache is warm), other
 * GAVs get `429 Too Many Requests` with `Retry-After: 60`. Non-`/listfiles`
 * paths return `None` from [[gavFromListfilesPath]] and are exempted
 * entirely.
 */
object CrawlerLimits:

  /**
   * Parse a request's path into a `/listfiles` GAV, or `None` for any other
   * route. Mirrors the two route shapes registered in
   * `webjars.routes.AppRoutes`:
   *
   *   - `/listfiles/<artifactId>/<version>`            — defaults groupId
   *     to `org.webjars` (the "classic" namespace).
   *   - `/listfiles/<groupId>/<artifactId>/<version>`  — explicit groupId.
   *
   * Path segments are URL-decoded so the parsed values match what the route
   * handlers see (the version segment in particular can be percent-encoded
   * when a crawler walks a sitemap with raw-versioned URLs).
   */
  def gavFromListfilesPath(request: Request): Option[GroupArtifactVersion] =
    def decode(s: String): String = URLDecoder.decode(s, "UTF-8").nn

    request.path.segments.toList match
      case "listfiles" :: a :: v :: Nil =>
        for
          aid <- ArtifactId.unapply(decode(a))
          ver <- Version.unapply(decode(v))
        yield GroupArtifactVersion(GroupId("org.webjars"), aid, ver)

      case "listfiles" :: g :: a :: v :: Nil =>
        for
          gid <- GroupId.unapply(decode(g))
          aid <- ArtifactId.unapply(decode(a))
          ver <- Version.unapply(decode(v))
        yield GroupArtifactVersion(gid, aid, ver)

      case _ =>
        None

  /** Layer the crawler-limiter middleware needs. */
  val layer: ZLayer[Any, Nothing, CrawlerLimiter[GroupArtifactVersion]] =
    CrawlerLimiter.layer[GroupArtifactVersion]

  /**
   * The middleware itself. Apply with
   * `routes @@ CrawlerLimits.middleware @@ Middleware.requestLogging(...)`
   * so that `requestLogging` is the outermost aspect — 429 responses still
   * appear in the request log.
   */
  val middleware: HandlerAspect[CrawlerLimiter[GroupArtifactVersion], Unit] =
    CrawlerLimiter.middleware[GroupArtifactVersion](gavFromListfilesPath)
