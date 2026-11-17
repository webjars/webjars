package webjars.utils

import com.augustnagro.magnum.*
import com.augustnagro.magnum.pg.PgCodec.given
import com.augustnagro.magnum.pg.json.JsonBDbCodec
import com.augustnagro.magnum.ziomagnum.*
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import webjars.models.WebJar.given
import webjars.models.{WebJar, WebJarVersion}
import webjars.utils.Db.given
import zio.*
import zio.direct.*
import zio.json.*

import java.time.{OffsetDateTime, ZoneOffset, ZonedDateTime}
import javax.sql.DataSource

object WebJarsCache:

  case class WebJarMeta(name: String, sourceUrl: String, versions: List[WebJarVersion], lastModified: ZonedDateTime)

  private given JsonBDbCodec[List[WebJarVersion]] with
    def encode(versions: List[WebJarVersion]): String = versions.toJson
    def decode(json: String): List[WebJarVersion] =
      json.fromJson[List[WebJarVersion]].fold(e => throw IllegalStateException(s"webjar.versions is not a version list: $e"), identity)

  private case class Row(artifactId: String, name: String, sourceUrl: String, versions: List[WebJarVersion], lastModified: OffsetDateTime) derives DbCodec:
    def meta: WebJarMeta = WebJarMeta(name, sourceUrl, versions, lastModified.toZonedDateTime)

  private val columns = sql"artifact_id, name, source_url, versions, last_modified"

  /** A cached WebJar as listed (e.g. the popular ranking's join). */
  private[utils] case class WebJarRow(groupId: String, artifactId: String, name: String, sourceUrl: String, versions: List[WebJarVersion]) derives DbCodec:
    def webJar: Option[WebJar] = WebJar.fromCache(groupId, artifactId, name, sourceUrl, versions)

  def getArtifact(groupArtifact: GroupArtifact): RIO[DataSource, Option[WebJarMeta]] =
    Db.withDataSource:
      sql"""select $columns from webjar
            where group_id = ${groupArtifact.groupId.toString} and artifact_id = ${groupArtifact.artifactId.toString}"""
        .zQuery[Row].map(_.headOption.map(_.meta))

  def getArtifacts(groupId: MavenCentral.GroupId, limit: Option[Int] = None, query: Option[String] = None): RIO[DataSource, Map[MavenCentral.ArtifactId, WebJarMeta]] =
    Db.withDataSource:
      sql"select $columns from webjar where group_id = ${groupId.toString} order by artifact_id".zQuery[Row].map: rows =>
        val filtered = query.fold(rows): q =>
          rows.filter: row =>
            row.artifactId.toLowerCase.contains(q.toLowerCase) || row.name.toLowerCase.contains(q.toLowerCase)
        limit.fold(filtered)(filtered.take).map(row => MavenCentral.ArtifactId(row.artifactId) -> row.meta).toMap

  def setArtifactDetails(groupArtifact: GroupArtifact, webJarMeta: WebJarMeta): RIO[DataSource, Unit] =
    // Defensive: refuse to write entries with empty artifactId. Older
    // zio-mavencentral parsing could leak `<a href="/">` directory
    // listings through as an empty artifactId. The table's check
    // constraint rejects them too; failing here names the artifact.
    if groupArtifact.artifactId.toString.isEmpty then
      ZIO.die(IllegalArgumentException(s"Refusing to cache artifact with empty artifactId in groupId ${groupArtifact.groupId}"))
    // A version-less entry can't be rendered or depended on, and the
    // discovery refresh only revisits artifacts missing from the cache, so
    // it would stay empty forever. Use `setOrTombstoneArtifact` instead.
    else if webJarMeta.versions.isEmpty then
      ZIO.die(IllegalArgumentException(s"Refusing to cache artifact $groupArtifact with no versions"))
    else
      Db.withDataSource:
        val lastModified = webJarMeta.lastModified.toOffsetDateTime
        sql"""insert into webjar (group_id, artifact_id, name, source_url, versions, last_modified)
              values (${groupArtifact.groupId.toString}, ${groupArtifact.artifactId.toString}, ${webJarMeta.name},
                      ${webJarMeta.sourceUrl}, ${webJarMeta.versions}, $lastModified)
              on conflict (group_id, artifact_id) do update set
                name = excluded.name, source_url = excluded.source_url,
                versions = excluded.versions, last_modified = excluded.last_modified""".zUpdate.unit

  def removeArtifact(groupArtifact: GroupArtifact): RIO[DataSource, Unit] =
    Db.withDataSource:
      sql"""delete from webjar
            where group_id = ${groupArtifact.groupId.toString} and artifact_id = ${groupArtifact.artifactId.toString}""".zUpdate.unit

  // When every version is gone (e.g. all tombstoned as ghosts), drop the
  // entry and tombstone the artifact; the expiry lets discovery retry it.
  def setOrTombstoneArtifact(groupArtifact: GroupArtifact, webJarMeta: WebJarMeta): RIO[DataSource, Unit] =
    if webJarMeta.versions.isEmpty then
      ZIO.logInfo(s"No usable versions for $groupArtifact; removing from cache and tombstoning") *>
        removeArtifact(groupArtifact) *>
        addTombstone(groupArtifact.groupId, groupArtifact.artifactId)
    else
      setArtifactDetails(groupArtifact, webJarMeta)

  /** Sets one version's numFiles. The row lock keeps concurrent updates of one artifact from losing each other's writes. */
  def updateVersion(groupArtifact: GroupArtifact, version: String, numFiles: Int): RIO[DataSource, Unit] =
    val groupId = groupArtifact.groupId.toString
    val artifactId = groupArtifact.artifactId.toString
    Db.withDataSource:
      transaction("update version"):
        defer:
          val row = sql"select $columns from webjar where group_id = $groupId and artifact_id = $artifactId for update"
            .zQuery[Row].map(_.headOption).someOrFail(RuntimeException(s"Artifact $groupArtifact not found in cache")).run
          val versions = row.versions.map(v => if v.number == version then v.copy(numFiles = Some(numFiles)) else v)
          sql"update webjar set versions = $versions where group_id = $groupId and artifact_id = $artifactId".zUpdate.unit.run

  // Tombstones: artifact ids that we tried to refresh and failed
  // (typically 404 on maven-metadata — orphan entries in the directory
  // listing). The refresher consults them to avoid re-attempting the
  // same broken IDs on every cycle. They expire after 7 days so artifacts
  // that come back eventually get retried.
  private val tombstoneTtl: Duration = 7.days

  def addTombstone(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): RIO[DataSource, Unit] =
    Db.withDataSource:
      sql"""insert into artifact_tombstone (group_id, artifact_id, expires_at)
            values (${groupId.toString}, ${artifactId.toString}, now() + ${Db.interval(tombstoneTtl)})
            on conflict (group_id, artifact_id) do update set expires_at = excluded.expires_at""".zUpdate.unit

  def getTombstones(groupId: MavenCentral.GroupId): RIO[DataSource, Set[MavenCentral.ArtifactId]] =
    Db.withDataSource:
      sql"select artifact_id from artifact_tombstone where group_id = ${groupId.toString} and expires_at > now()"
        .zQuery[String].map(_.map(MavenCentral.ArtifactId(_)).toSet)

  // Version-level tombstones: GAVs where Maven Central's metadata lists
  // the version but the underlying jar doesn't actually exist. The
  // canonical case is a broken publish — a POM was uploaded but the jar
  // upload failed and was never recovered (e.g. `org.webjars.npm:killable:1.0.1`,
  // which has only `killable-1.0.1.pom` in its directory).
  //
  // The signal is structured: `WebJarsFileService.getNumFiles` returns
  // a `FileNotFoundException` (status 404 from the file-service) only
  // when the file-service can't find the jar on Maven Central. Other
  // errors (5xx, transport) become a generic Exception, so we don't
  // confuse "transiently unreachable" with "permanently absent".
  //
  // When we detect a ghost, we tombstone it AND remove the
  // version from `WebJarMeta.versions` — so it stops appearing in
  // search/popular/list/all/artifact-detail pages, and the
  // periodic numFiles backfill stops retrying it. 7-day expiry: if a
  // missing jar is ever re-published, the tombstone expires and
  // discovery picks it back up.
  private val versionTombstoneTtl: Duration = 7.days

  def addVersionTombstone(ga: GroupArtifact, version: String): RIO[DataSource, Unit] =
    Db.withDataSource:
      sql"""insert into version_tombstone (group_id, artifact_id, version, expires_at)
            values (${ga.groupId.toString}, ${ga.artifactId.toString}, $version, now() + ${Db.interval(versionTombstoneTtl)})
            on conflict (group_id, artifact_id, version) do update set expires_at = excluded.expires_at""".zUpdate.unit

  def getVersionTombstones(ga: GroupArtifact): RIO[DataSource, Set[String]] =
    Db.withDataSource:
      sql"""select version from version_tombstone
            where group_id = ${ga.groupId.toString} and artifact_id = ${ga.artifactId.toString} and expires_at > now()"""
        .zQuery[String].map(_.toSet)

  def removeVersionTombstones(ga: GroupArtifact, versions: Set[String]): RIO[DataSource, Unit] =
    ZIO.foreachDiscard(NonEmptyChunk.fromIterableOption(versions)): toRemove =>
      Db.withDataSource:
        val versionList: Seq[String] = toRemove.toList
        sql"""delete from version_tombstone
              where group_id = ${ga.groupId.toString} and artifact_id = ${ga.artifactId.toString} and version = any($versionList::text[])"""
          .zUpdate.unit

  // Pending deploys: GAVs we've just published via this app, awaiting Maven
  // Central propagation (~1hr). `added_at` comes from the ZIO `Clock` so
  // stuck entries age out (`cullStalePendingDeploys`); re-adding an entry
  // restarts its clock.
  case class PendingDeploy(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version)

  private case class PendingRow(groupId: String, artifactId: String, version: String) derives DbCodec:
    def pendingDeploy: PendingDeploy =
      PendingDeploy(MavenCentral.GroupId(groupId), MavenCentral.ArtifactId(artifactId), MavenCentral.Version(version))

  private val now: UIO[OffsetDateTime] =
    Clock.currentDateTime.map(_.withOffsetSameInstant(ZoneOffset.UTC))

  def addPendingDeploy(p: PendingDeploy): RIO[DataSource, Unit] =
    defer:
      val addedAt = now.run
      Db.withDataSource:
        sql"""insert into pending_deploy (group_id, artifact_id, version, added_at)
              values (${p.groupId.toString}, ${p.artifactId.toString}, ${p.version.toString}, $addedAt)
              on conflict (group_id, artifact_id, version) do update set added_at = excluded.added_at""".zUpdate.unit
      .run

  def getPendingDeploys: RIO[DataSource, Set[PendingDeploy]] =
    Db.withDataSource:
      sql"select group_id, artifact_id, version from pending_deploy".zQuery[PendingRow].map(_.map(_.pendingDeploy).toSet)

  def removePendingDeploy(p: PendingDeploy): RIO[DataSource, Unit] =
    Db.withDataSource:
      sql"""delete from pending_deploy
            where group_id = ${p.groupId.toString} and artifact_id = ${p.artifactId.toString} and version = ${p.version.toString}"""
        .zUpdate.unit

  // A just-deployed version is listed in maven-metadata before its jar is
  // reachable through the file-service, so a 404 for it is expected and
  // must not be treated as a ghost.
  def getPendingVersions(ga: GroupArtifact): RIO[DataSource, Set[String]] =
    Db.withDataSource:
      sql"""select version from pending_deploy
            where group_id = ${ga.groupId.toString} and artifact_id = ${ga.artifactId.toString}"""
        .zQuery[String].map(_.toSet)

  // Drop entries older than the given age (e.g., a deploy that never made
  // it to MC after 48h is probably abandoned).
  def cullStalePendingDeploys(maxAge: Duration): RIO[DataSource, Long] =
    defer:
      val cutoff = now.run.minus(maxAge)
      Db.withDataSource(sql"delete from pending_deploy where added_at < $cutoff".zUpdate).run.toLong
