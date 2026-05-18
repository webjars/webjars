package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import webjars.models.WebJar
import webjars.models.WebJar.given
import webjars.utils.WebJarsCache
import zio.redis.Redis
import zio.*
import zio.http.*
import zio.json.*

import java.time.ZonedDateTime

// Hydrate the local WebJarsCache by fetching /all from the live production
// site. Avoids checking a multi-megabyte snapshot into git or needing prod
// Redis credentials — the JSON response is publicly served, always fresh,
// and downloads in ~1s.
//
// `lastModified` isn't surfaced by /all and the test app never runs the
// refresh loop, so we synthesize `ZonedDateTime.now()` — the field is only
// consulted by `MavenCentral.isModifiedSince` during the refresh cycle.
object CacheHydrate:

  val DefaultUrl: String = "https://www.webjars.org/all"

  def fromUrl(
    url: String,
    filter: WebJar => Boolean = _ => true,
  ): ZIO[Client & Redis, Throwable, Int] =
    for
      _        <- ZIO.logInfo(s"Hydrating local cache from $url")
      request   = Request.get(url).addHeader(Header.Accept(MediaType.application.json))
      response <- ZIO.serviceWithZIO[Client](_.batched(request))
      _        <- ZIO.unless(response.status.isSuccess):
                    ZIO.fail(RuntimeException(s"GET $url returned ${response.status.code}"))
      body     <- response.body.asString
      webjars  <- ZIO.fromEither(body.fromJson[List[WebJar]]).mapError(msg => RuntimeException(s"failed to parse /all JSON: $msg"))
      filtered  = webjars.filter(filter)
      now      <- Clock.currentDateTime.map(_.toZonedDateTime)
      writes    = ZIO.foreachDiscard(filtered): wj =>
                    WebJarsCache.setArtifactDetails(
                      GroupArtifact(MavenCentral.GroupId(wj.groupId), MavenCentral.ArtifactId(wj.artifactId)),
                      WebJarsCache.WebJarMeta(wj.name, wj.sourceUrl, wj.versions.toList, now),
                    )
      _        <- writes
      _        <- ZIO.logInfo(s"Hydrated ${filtered.size}/${webjars.size} artifacts from $url")
    yield filtered.size
