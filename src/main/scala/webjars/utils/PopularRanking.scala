package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import webjars.models.WebJar
import zio.*
import zio.redis.*

import java.time.ZoneOffset

// Resolves the "Popular WebJars" list once per dyno lifetime from the daily
// `popular:<YYYY-MM-DD>` ZSETs written by PopularMetrics. The aggregated top
// list is held in a Ref and serves /  and /popular for the whole dyno (~24h
// on Heroku) — no mid-run refresh.
//
// Cold start: when the 30-day window is empty (first deploy, freshly flushed
// Valkey), fall back to the legacy "first 20 from HGETALL per group" view so
// the home page is never blank. Until populate() finishes, snapshot returns
// Nil and the view layer is expected to render an "indexing" placeholder.
trait PopularRanking:
  def snapshot: UIO[List[WebJar]]
  def populate: URIO[Redis, Unit]

case class PopularRankingLive(
  ref:            Ref[List[WebJar]],
  allDeployables: AllDeployables,
) extends PopularRanking:

  private val WindowDays       = 30
  private val AggKey           = "popular:agg"
  private val TopN             = 40
  private val FallbackPerGroup = 20

  def snapshot: UIO[List[WebJar]] = ref.get

  private val dailyKeys: UIO[List[String]] =
    Clock.currentDateTime.map: now =>
      val today = now.atZoneSameInstant(ZoneOffset.UTC).toLocalDate
      (0 until WindowDays).toList.map(i => s"popular:${today.minusDays(i.toLong)}")

  private def hydrate(member: String): ZIO[Redis, Throwable, Option[WebJar]] =
    member.split(":", 2) match
      case Array(gid, aid) =>
        WebJarsCache
          .getArtifact(GroupArtifact(MavenCentral.GroupId(gid), MavenCentral.ArtifactId(aid)))
          .map(_.map(meta => WebJar(gid, aid, meta.name, meta.sourceUrl, meta.versions.toSeq)))
      case _ =>
        ZIO.none

  private val aggregateAndHydrate: ZIO[Redis, Throwable, List[WebJar]] =
    for
      keys    <- dailyKeys
      // ZUNIONSTORE popular:agg N popular:<d0> popular:<d-1> ... popular:<d-29>.
      // Missing daily keys count as empty sets, default aggregate is SUM.
      _       <- ZIO.serviceWithZIO[Redis](_.zUnionStore(AggKey, keys.head, keys.tail*)())
      ranked  <- ZIO.serviceWithZIO[Redis](_.zRangeWithScores(
                   AggKey,
                   SortedSetRange.Range(RangeMinimum(0), RangeMaximum.Inclusive((TopN - 1).toLong)),
                   rev = true,
                 ).returning[String])
      webjars <- ZIO.foreach(ranked.toList)(ms => hydrate(ms.member))
    yield webjars.flatten

  private val coldStartFallback: ZIO[Redis, Throwable, List[WebJar]] =
    ZIO.foreach(allDeployables.groupIds().toList): groupId =>
      WebJarsCache.getArtifacts(groupId, Some(FallbackPerGroup)).map: m =>
        m.toSeq.map: (artifactId, meta) =>
          WebJar(groupId.toString, artifactId.toString, meta.name, meta.sourceUrl, meta.versions.toSeq)
    .map(_.flatten)

  // True ⇒ the Ref was set; False ⇒ both the aggregate and the per-group
  // fallback came back empty (e.g. WebJarsCache hasn't been populated yet by
  // the Maven Central refresh loop), so the caller should try again.
  private val tryPopulate: ZIO[Redis, Throwable, Boolean] =
    for
      ranked      <- aggregateAndHydrate
      usedFallback = ranked.isEmpty
      chosen      <- if usedFallback then coldStartFallback else ZIO.succeed(ranked)
      _           <- ZIO.when(chosen.nonEmpty):
                       ref.set(chosen) *>
                         ZIO.logInfo(s"PopularRanking populated: ${chosen.size} artifacts (fallback=$usedFallback)")
    yield chosen.nonEmpty

  def populate: URIO[Redis, Unit] =
    // The Maven Central refresh loop runs asynchronously after boot — until
    // it hydrates WebJarsCache, both the aggregate and the fallback are
    // empty. Poll every 5s until we get something, then stop. (Once `chosen`
    // is non-empty the Ref is set and the snapshot stays valid for the
    // dyno's lifetime; Heroku cycles dynos daily so no further refresh.)
    def loop: ZIO[Redis, Throwable, Unit] =
      tryPopulate.flatMap:
        case true  => ZIO.unit
        case false => ZIO.logInfo("PopularRanking: cache not ready, retrying in 5s") *> loop.delay(5.seconds)

    loop
      .tapErrorCause(c => ZIO.logErrorCause("PopularRanking populate failed", c))
      .ignore

object PopularRanking:
  val live: ZLayer[AllDeployables, Nothing, PopularRanking] =
    ZLayer.scoped:
      for
        allDeployables <- ZIO.service[AllDeployables]
        ref            <- Ref.make(List.empty[WebJar])
      yield PopularRankingLive(ref, allDeployables)
