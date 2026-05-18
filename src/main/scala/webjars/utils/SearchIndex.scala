package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.models.WebJar
import zio.*
import zio.redis.Redis

// In-memory cache of fully-hydrated WebJar records for every artifact. The
// `/search`, `/all`, and `/list/:groupId` routes read from this Ref and never
// touch Valkey on the user path. Rebuild is event-driven: startup, after each
// `MavenCentralWebJars.refreshAll` cycle, and on demand. The Ref starts empty
// (Nil); the view layer uses that to render an "Indexing artifacts" message
// until the first rebuild completes.
//
// Rebuild reads from the existing `WebJarsCache` per-group hash via `HGETALL`
// — once per trigger, not per request — and assembles `List[WebJar]` sorted
// by (groupId, artifactId). At ~5000 artifacts this is a few MB and well
// under a second to build.
trait SearchIndex:
  def snapshot: UIO[List[WebJar]]
  def rebuild:  URIO[Redis, Unit]

case class SearchIndexLive(
  ref:             Ref[List[WebJar]],
  allDeployables:  AllDeployables,
) extends SearchIndex:

  def snapshot: UIO[List[WebJar]] = ref.get

  def rebuild: URIO[Redis, Unit] =
    val build: ZIO[Redis, Throwable, List[WebJar]] =
      ZIO.foreachPar(allDeployables.groupIds().toSeq): groupId =>
        WebJarsCache.getArtifacts(groupId).map: artifactsMap =>
          artifactsMap.toSeq.map: (artifactId, meta) =>
            WebJar(groupId.toString, artifactId.toString, meta.name, meta.sourceUrl, meta.versions.toSeq)
      .map(_.flatten.sortBy(wj => (wj.groupId, wj.artifactId)).toList)

    build
      .flatMap(list => ref.set(list) *> ZIO.logInfo(s"SearchIndex rebuilt: ${list.size} artifacts"))
      .tapErrorCause(c => ZIO.logErrorCause("SearchIndex rebuild failed", c))
      .ignore

object SearchIndex:
  val live: ZLayer[AllDeployables, Nothing, SearchIndex] =
    ZLayer.scoped:
      for
        allDeployables <- ZIO.service[AllDeployables]
        ref            <- Ref.make(List.empty[WebJar])
      yield SearchIndexLive(ref, allDeployables)
