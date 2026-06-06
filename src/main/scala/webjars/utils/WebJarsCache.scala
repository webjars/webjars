package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import webjars.models.WebJarVersion
import zio.*
import zio.direct.*
import zio.redis.*
import zio.schema.{Schema, derived}

import java.time.ZonedDateTime

object WebJarsCache:

  case class WebJarMeta(name: String, sourceUrl: String, versions: List[WebJarVersion], lastModified: ZonedDateTime) derives Schema

  def getArtifact(groupArtifact: GroupArtifact): ZIO[Redis, RedisError, Option[WebJarMeta]] =
    ZIO.serviceWithZIO[Redis]: redis =>
      redis.hGet(groupArtifact.groupId, groupArtifact.artifactId).returning[WebJarMeta]

  def getArtifacts(groupId: MavenCentral.GroupId, limit: Option[Int] = None, query: Option[String] = None): ZIO[Redis, RedisError, Map[MavenCentral.ArtifactId, WebJarMeta]] =
    ZIO.serviceWithZIO[Redis]: redis =>
      val filtered = redis.hGetAll(groupId).returning[MavenCentral.ArtifactId, WebJarMeta].map: webJars =>
        query.fold(webJars): q =>
          webJars.filter: (artifactId, meta) =>
            artifactId.toString.toLowerCase.contains(q.toLowerCase) ||
              meta.name.toLowerCase.contains(q.toLowerCase)

      filtered.map: webJars =>
        limit.fold(webJars)(webJars.take)

  def setArtifactDetails(groupArtifact: GroupArtifact, webJarMeta: WebJarMeta): ZIO[Redis, Throwable, Unit] =
    // Defensive: refuse to write entries with empty artifactId. Older
    // zio-mavencentral parsing could leak `<a href="/">` directory
    // listings through as an empty artifactId, producing corrupted
    // hash entries that ate up the periodic numFiles backfill loop
    // because `searchVersions(groupId, "")` resolved to the
    // groupId-as-artifact's `maven-metadata.xml` instead of failing.
    // The current library version filters these correctly at the
    // discovery boundary, but this guard ensures the cache itself
    // can't be corrupted by any future regression at any other
    // upstream layer.
    if groupArtifact.artifactId.toString.isEmpty then
      ZIO.die(IllegalArgumentException(s"Refusing to cache artifact with empty artifactId in groupId ${groupArtifact.groupId}"))
    else
      ZIO.serviceWithZIO[Redis]: redis =>
        redis.hSet(groupArtifact.groupId, groupArtifact.artifactId -> webJarMeta).unit

  def updateVersion(groupArtifact: GroupArtifact, version: String, numFiles: Int): ZIO[Redis, Throwable, Unit] =
    defer:
      val artifact = getArtifact(groupArtifact).someOrFail(RuntimeException(s"Artifact $groupArtifact not found in cache")).run
      val updatedVersions = artifact.versions.map: v =>
        if v.number == version then
          v.copy(numFiles = Some(numFiles))
        else
          v
      val updated = artifact.copy(versions = updatedVersions)
      setArtifactDetails(groupArtifact, updated).run

  // Tombstones: artifact ids that we tried to refresh and failed
  // (typically 404 on maven-metadata — orphan entries in the directory
  // listing). The refresher consults this set to avoid re-attempting the
  // same broken IDs on every cycle. 7-day TTL so artifacts that come back
  // eventually get retried.
  private def tombstoneKey(groupId: MavenCentral.GroupId): String = s"tombstone:${groupId.toString}"
  private val tombstoneTtl: Duration = 7.days

  def addTombstone(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Redis, Throwable, Unit] =
    ZIO.serviceWithZIO[Redis]: redis =>
      val key = tombstoneKey(groupId)
      redis.sAdd(key, artifactId.toString) *>
        redis.expire(key, tombstoneTtl).unit

  def getTombstones(groupId: MavenCentral.GroupId): ZIO[Redis, Throwable, Set[MavenCentral.ArtifactId]] =
    ZIO.serviceWithZIO[Redis]: redis =>
      redis.sMembers(tombstoneKey(groupId)).returning[String]
        .map(_.map(MavenCentral.ArtifactId(_)).toSet)

  // Pending deploys: GAVs we've just published via this app, awaiting Maven
  // Central propagation (~1hr). Stored as a ZSET with epoch-second scores
  // so we can age out stuck entries with ZREMRANGEBYSCORE. Members use `|`
  // as the separator since `:` already appears in groupIds like
  // `org.webjars.npm`.
  case class PendingDeploy(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version)

  private val PendingDeploysKey = "pending_deploys"

  private def encodePending(p: PendingDeploy): String =
    s"${p.groupId.toString}|${p.artifactId.toString}|${p.version.toString}"

  private def decodePending(s: String): Option[PendingDeploy] =
    s.split('|') match
      case Array(g, a, v) => Some(PendingDeploy(MavenCentral.GroupId(g), MavenCentral.ArtifactId(a), MavenCentral.Version(v)))
      case _              => None

  def addPendingDeploy(p: PendingDeploy): ZIO[Redis, Throwable, Unit] =
    for
      now   <- Clock.currentDateTime.map(_.toInstant.getEpochSecond)
      score  = now.toDouble
      _     <- ZIO.serviceWithZIO[Redis](_.zAdd(PendingDeploysKey)(MemberScore(encodePending(p), score)).unit)
    yield ()

  def getPendingDeploys: ZIO[Redis, Throwable, Set[PendingDeploy]] =
    ZIO.serviceWithZIO[Redis]: redis =>
      redis
        .zRange(PendingDeploysKey, SortedSetRange.Range(RangeMinimum(0), RangeMaximum.Inclusive(-1)))
        .returning[String]
        .map(_.toList.flatMap(decodePending).toSet)

  def removePendingDeploy(p: PendingDeploy): ZIO[Redis, Throwable, Unit] =
    ZIO.serviceWithZIO[Redis](_.zRem(PendingDeploysKey, encodePending(p)).unit)

  // Drop entries older than the given age (e.g., a deploy that never made
  // it to MC after 48h is probably abandoned).
  def cullStalePendingDeploys(maxAge: Duration): ZIO[Redis, Throwable, Long] =
    for
      now    <- Clock.currentDateTime.map(_.toInstant.getEpochSecond)
      cutoff  = (now - maxAge.getSeconds).toDouble
      removed <- ZIO.serviceWithZIO[Redis](_.zRemRangeByScore(
                  PendingDeploysKey,
                  SortedSetRange.ScoreRange(ScoreMinimum.Infinity, ScoreMaximum.Open(cutoff)),
                ))
    yield removed
