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
