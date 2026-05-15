package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.models.{WebJar, WebJarVersion}
import zio.*
import zio.direct.*
import zio.http.Client
import zio.redis.Redis

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import scala.xml.Elem

trait MavenCentralWebJars:
  def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Elem]
  private[utils] def refreshGroup(groupId: MavenCentral.GroupId, updateNumFiles: Boolean = true): ZIO[Client & Redis, Throwable, Set[MavenCentral.ArtifactId]]
  def refreshAll(groupIds: Set[MavenCentral.GroupId]): ZIO[Client & Redis, Nothing, Unit]
  def startRefreshLoop(): ZIO[Any, Nothing, Fiber.Runtime[Nothing, Unit]]
  def featuredWebJars(groupId: MavenCentral.GroupId, limit: Int): ZIO[Any, Throwable, Seq[WebJar]]
  def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): ZIO[Any, Throwable, Seq[WebJar]]

case class MavenCentralWebJarsLive(config: AppConfig, webJarsFileService: WebJarsFileService, valkey: Valkey, allDeployables: AllDeployables) extends MavenCentralWebJars:

  private[utils] lazy val maybeLimit: Option[Int] = config.mavenCentralLimit

  private val mavenCentralLayer: ZLayer[Any, Nothing, Scope & Client] = Client.default.orDie ++ Scope.default

  def fetchPom(gav: MavenCentral.GroupArtifactVersion): ZIO[Scope, Throwable, Elem] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version).mapError {
      case _: MavenCentral.NotFoundError => FileNotFoundException("pom not found")
      case t: Throwable => t
    }.provide(mavenCentralLayer)

  private def fetchWebJarNameAndUrl(gav: MavenCentral.GroupArtifactVersion): ZIO[Client & Scope, Nothing, (String, String)] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version)
      .flatMap: xml =>
        val artifactId = (xml \ "artifactId").text
        val rawName = (xml \ "name").text
        val name = if rawName.contains("${") || rawName.isEmpty then artifactId else rawName

        val rawUrl = (xml \ "scm" \ "url").text

        val url = if rawUrl.contains("${") then
          ZIO.fail(new RuntimeException("Unable to determine source url"))
        else
          if rawUrl != "" then
            ZIO.succeed(rawUrl)
          else
            val parentGroupId = (xml \ "parent" \ "groupId").text
            val parentArtifactId = (xml \ "parent" \ "artifactId").text
            val parentVersion = (xml \ "parent" \ "version").text
            MavenCentral.pom(MavenCentral.GroupId(parentGroupId), MavenCentral.ArtifactId(parentArtifactId), MavenCentral.Version(parentVersion)).map { parentXml =>
              (parentXml \ "scm" \ "url").text
            }

        url.map(name -> _)
      .catchAll: _ =>
        ZIO.succeed:
          gav.artifactId.toString -> s"https://github.com/webjars/${gav.artifactId}"

  private def fetchVersions(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Scope, Throwable, MavenCentral.WithCacheInfo[Seq[MavenCentral.Version]]] =
    MavenCentral.searchVersions(groupId, artifactId).mapError {
      case _: MavenCentral.GroupIdOrArtifactIdNotFoundError => Throwable("groupId or artifactId not found")
      case t: Throwable => t
    }

  private def fetchArtifactIds(groupId: MavenCentral.GroupId): ZIO[Client & Scope, Throwable, MavenCentral.WithCacheInfo[Seq[MavenCentral.ArtifactId]]] =
    MavenCentral.searchArtifacts(groupId).mapBoth({
      case _: MavenCentral.GroupIdNotFoundError => Throwable("groupId not found")
      case t: Throwable => t
    }, {
      artifacts =>
        artifacts.copy(value = artifacts.value.filterNot { artifactId =>
          artifactId.toString.startsWith("webjars-") ||
            artifactId.toString == "bower" ||
            artifactId.toString == "bowergithub" ||
            artifactId.toString == "2.11.2"
        })
    })

  private def refreshArtifact(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, versions: Seq[WebJarVersion], updateNumFiles: Boolean): ZIO[Client & Redis & Scope, Throwable, Unit] =
    ZIO.logInfo(s"Refreshing artifact: $groupId:$artifactId") *> {
      val refresh = defer:
        val versionsResult = fetchVersions(groupId, artifactId).run
        val latestVersion = versionsResult.value.head
        val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, latestVersion)
        val (name, url) = fetchWebJarNameAndUrl(gav).run
        WebJarsCache.setArtifactDetails(
          gav.noVersion,
          WebJarsCache.WebJarMeta(name, url, versionsResult.value.map(v => WebJarVersion(v.toString, None)).toList, versionsResult.maybeLastModified.getOrElse(ZonedDateTime.now()))
        ).run

        val resolvedVersions = versions.flatMap: webJarVersion =>
          webJarVersion.numFiles.map(_ => webJarVersion.number)

        val versionsToRefresh = versionsResult.value.diff(resolvedVersions)

        ZIO.when(updateNumFiles):
          ZIO.foreachPar(versionsToRefresh) { version =>
            webJarsFileService.getNumFiles(gav.copy(version = version)).flatMap { numFiles =>
              WebJarsCache.updateVersion(gav.noVersion, version.toString, numFiles)
            }
          }.withParallelism(5).unit
          .forkDaemon
        .unit.run

      refresh.tapError(error => ZIO.logError(s"Error refreshing artifact $groupId:$artifactId: ${error.getMessage}"))
    }

  private[utils] def refreshGroup(groupId: MavenCentral.GroupId, updateNumFiles: Boolean = true): ZIO[Client & Redis, Throwable, Set[MavenCentral.ArtifactId]] =
    ZIO.logInfo(s"Refreshing groupId: $groupId with limit ${maybeLimit.getOrElse("none")} and updateNumFiles: $updateNumFiles") *>
    ZIO.scoped:
      defer:
        val artifactsResult = fetchArtifactIds(groupId).run
        val artifactsWithLimit = maybeLimit.fold(artifactsResult.value)(artifactsResult.value.take)
        val cachedArtifacts = WebJarsCache.getArtifacts(groupId).run
        val missingArtifacts = artifactsWithLimit.diff(cachedArtifacts.keys.toSeq).toSet
        val refreshMissing = ZIO.foreachPar(missingArtifacts.toSeq) { artifactId =>
          refreshArtifact(groupId, artifactId, Seq.empty, updateNumFiles).as(artifactId).option
        }.withParallelism(10).map(_.flatten.toSet).run
        val cachedArtifactsWithLimit = maybeLimit.fold(cachedArtifacts)(cachedArtifacts.take)
        val modifiedArtifacts = ZIO.foreachPar(cachedArtifactsWithLimit.toSeq) { case (artifactId, meta) =>
          MavenCentral.isModifiedSince(meta.lastModified, groupId, Some(artifactId))
            .orElseSucceed(false)
            .map(modified => Option.when(modified)(artifactId -> meta))
        }.withParallelism(20).map(_.flatten).run
        val refreshUpdated = ZIO.foreachPar(modifiedArtifacts) { case (artifactId, meta) =>
          refreshArtifact(groupId, artifactId, meta.versions.toSeq, updateNumFiles).as(artifactId).option
        }.withParallelism(10).map(_.flatten.toSet).run
        (refreshMissing ++ refreshUpdated).toSet

  def refreshAll(groupIds: Set[MavenCentral.GroupId]): ZIO[Client & Redis, Nothing, Unit] =
    ZIO.foreachDiscard(groupIds):
      groupId => refreshGroup(groupId).ignoreLogged

  def startRefreshLoop(): ZIO[Any, Nothing, Fiber.Runtime[Nothing, Unit]] =
    val once = refreshAll(allDeployables.groupIds())
    val effect = config.mavenCentralRefreshInterval.fold(once)(interval => once.repeat(Schedule.spaced(interval)).unit)
    effect.provide(Client.default.orDie ++ valkey.layer).forkDaemon

  extension (m: Map[MavenCentral.ArtifactId, WebJarsCache.WebJarMeta])
    private def toWebJars(groupId: MavenCentral.GroupId): Seq[WebJar] =
      m.map: (artifactId, meta) =>
        WebJar(groupId.toString, artifactId.toString, meta.name, meta.sourceUrl, meta.versions.toSeq)
      .toSeq

  def featuredWebJars(groupId: MavenCentral.GroupId, limit: Int): ZIO[Any, Throwable, Seq[WebJar]] =
    WebJarsCache.getArtifacts(groupId, Some(limit)).map(_.toWebJars(groupId)).provide(valkey.layer)

  def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): ZIO[Any, Throwable, Seq[WebJar]] =
    WebJarsCache.getArtifacts(groupId, maybeLimit, query).map(_.toWebJars(groupId)).provide(valkey.layer)

object MavenCentralWebJars:
  val live: ZLayer[AppConfig & WebJarsFileService & Valkey & AllDeployables, Nothing, MavenCentralWebJars] = ZLayer.derive[MavenCentralWebJarsLive]
