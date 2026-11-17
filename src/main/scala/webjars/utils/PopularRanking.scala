package webjars.utils

import com.augustnagro.magnum.*
import com.augustnagro.magnum.ziomagnum.*
import webjars.models.WebJar
import webjars.utils.Db.given
import zio.*

import javax.sql.DataSource

// Resolves the "Popular WebJars" list from the `popular_daily` scores
// written by PopularMetrics. The aggregated top list is held in a Ref and
// serves / and /popular; `rebuild` refreshes it every refresh cycle.
//
// Cold start: when the 30-day window is empty (first deploy, fresh
// database), fall back to "the first 20 artifacts per group" so the home
// page is never blank. Until populate() finishes, snapshot returns Nil and
// the view layer is expected to render an "indexing" placeholder.
trait PopularRanking:
  def snapshot: UIO[List[WebJar]]
  def populate: URIO[DataSource, Unit]
  /** One-shot rebuild from the current `WebJarsCache` state. Unlike
   *  [[populate]], this does NOT retry when the underlying cache is
   *  empty — it's intended to be called from the periodic refresh
   *  cycle, after the cache has already been hydrated, so that
   *  numFiles backfills (and any other artifact updates) propagate
   *  into the popular-ranking snapshot serving `/` and `/popular`.
   *
   *  Without this, the snapshot is frozen at the values present when
   *  [[populate]] first succeeded — typically once per dyno lifetime
   *  on Heroku. Updates to `WebJarsCache` after that point would not
   *  be reflected on the home page until the next dyno cycle. */
  def rebuild:  URIO[DataSource, Unit]

case class PopularRankingLive(
  ref:            Ref[List[WebJar]],
  allDeployables: AllDeployables,
) extends PopularRanking:

  private val WindowDays       = 30
  private val TopN             = 40
  private val FallbackPerGroup = 20

  def snapshot: UIO[List[WebJar]] = ref.get

  // Artifacts no longer in the cache drop out of the ranking (the join).
  private val aggregateAndHydrate: RIO[DataSource, List[WebJar]] =
    PopularMetrics.today.flatMap: today =>
      val since = today.minusDays((WindowDays - 1).toLong)
      Db.withDataSource:
        sql"""select p.group_id, p.artifact_id, w.name, w.source_url, w.versions
              from (select group_id, artifact_id, sum(score) as total from popular_daily
                    where day >= $since group by group_id, artifact_id) p
              join webjar w using (group_id, artifact_id)
              order by p.total desc, p.group_id, p.artifact_id
              limit $TopN""".zQuery[WebJarsCache.WebJarRow]
      .map(_.toList.flatMap(_.webJar))

  private val coldStartFallback: RIO[DataSource, List[WebJar]] =
    ZIO.foreach(allDeployables.groupIds().toList): groupId =>
      WebJarsCache.getArtifacts(groupId, Some(FallbackPerGroup)).map: m =>
        m.toSeq.flatMap: (artifactId, meta) =>
          WebJar.fromCache(groupId.toString, artifactId.toString, meta.name, meta.sourceUrl, meta.versions)
    .map(_.flatten)

  // True ⇒ the Ref was set; False ⇒ both the aggregate and the per-group
  // fallback came back empty (e.g. WebJarsCache hasn't been populated yet by
  // the Maven Central refresh loop), so the caller should try again.
  private val tryPopulate: RIO[DataSource, Boolean] =
    for
      ranked      <- aggregateAndHydrate
      usedFallback = ranked.isEmpty
      chosen      <- if usedFallback then coldStartFallback else ZIO.succeed(ranked)
      _           <- ZIO.when(chosen.nonEmpty):
                       ref.set(chosen) *>
                         ZIO.logInfo(s"PopularRanking populated: ${chosen.size} artifacts (fallback=$usedFallback)")
    yield chosen.nonEmpty

  def populate: URIO[DataSource, Unit] =
    // The Maven Central refresh loop runs asynchronously after boot — until
    // it hydrates WebJarsCache, both the aggregate and the fallback are
    // empty. Poll every 5s until we get something, then stop; `rebuild`
    // keeps it current from then on.
    def loop: RIO[DataSource, Unit] =
      tryPopulate.flatMap:
        case true  => ZIO.unit
        case false => ZIO.logInfo("PopularRanking: cache not ready, retrying in 5s") *> loop.delay(5.seconds)

    loop
      .tapErrorCause(c => ZIO.logErrorCause("PopularRanking populate failed", c))
      .ignore

  def rebuild: URIO[DataSource, Unit] =
    // One-shot: re-aggregate from WebJarsCache and update the Ref iff the
    // result is non-empty. Empty results (e.g. transient database blip) leave
    // the prior snapshot in place — preferable to wiping the home page.
    tryPopulate
      .tapErrorCause(c => ZIO.logErrorCause("PopularRanking rebuild failed", c))
      .ignore

object PopularRanking:
  val live: ZLayer[AllDeployables, Nothing, PopularRanking] =
    ZLayer.scoped:
      for
        allDeployables <- ZIO.service[AllDeployables]
        ref            <- Ref.make(List.empty[WebJar])
      yield PopularRankingLive(ref, allDeployables)
