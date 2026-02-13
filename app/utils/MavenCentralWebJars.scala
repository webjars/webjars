package utils

import com.google.inject.ImplementedBy
import com.jamesward.zio_mavencentral.MavenCentral
import models.{WebJar, WebJarVersion}
import play.api.{Configuration, Environment, Logging, Mode}
import utils.Adapter.*
import zio.*
import zio.http.Client
import zio.redis.Redis

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.language.postfixOps
import scala.xml.Elem

@ImplementedBy(classOf[MavenCentralWebJarsLive])
trait MavenCentralWebJars:
  def fetchPom(gav: MavenCentral.GroupArtifactVersion): Future[Elem]
  def featuredWebJars(groupId: MavenCentral.GroupId, limit: Int): Future[Seq[WebJar]]
  def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): Future[Seq[WebJar]]

@Singleton
class MavenCentralWebJarsLive @Inject() (configuration: Configuration, webJarsFileService: WebJarsFileService, environment: Environment, valkey: Valkey, allDeployables: AllDeployables) extends MavenCentralWebJars with Logging {

  private[utils] lazy val maybeLimit = configuration.getOptional[Int]("mavencentral.limit").orElse(Option.when(environment.mode == Mode.Dev)(5))

  extension (gav: MavenCentral.GroupArtifactVersion)
    def cacheKey = s"${gav.groupId}-${gav.artifactId}-${gav.version}"

  private val mavenCentralLayer: ZLayer[Any, Nothing, Scope & Client] = Client.default.orDie ++ Scope.default

  override def fetchPom(gav: MavenCentral.GroupArtifactVersion): Future[Elem] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version).mapError {
      case e: MavenCentral.NotFoundError => FileNotFoundException("pom not found")
      case t: Throwable => t
    }.runToFuture(mavenCentralLayer)


  private def fetchWebJarNameAndUrl(gav: MavenCentral.GroupArtifactVersion): ZIO[Client & Scope, Nothing, (String, String)] =
    MavenCentral.pom(gav.groupId, gav.artifactId, gav.version)
      .flatMap: xml =>
        val artifactId = (xml \ "artifactId").text
        val rawName = (xml \ "name").text
        val name = if (rawName.contains("${") || rawName.isEmpty) {
          // can't handle pom properties so fallback to id
          artifactId
        } else {
          rawName
        }

        val rawUrl = (xml \ "scm" \ "url").text

        val url = if (rawUrl.contains("${")) {
          // can't handle pom properties so fallback to a guess
          ZIO.fail(new RuntimeException("Unable to determine source url"))
        } else {
          if (rawUrl != "") {
            ZIO.succeed(rawUrl)
          }
          else {
            // try the parent pom
            val parentGroupId = (xml \ "parent" \ "groupId").text
            val parentArtifactId = (xml \ "parent" \ "artifactId").text
            val parentVersion = (xml \ "parent" \ "version").text

            MavenCentral.pom(MavenCentral.GroupId(parentGroupId), MavenCentral.ArtifactId(parentArtifactId), MavenCentral.Version(parentVersion)).map { parentXml =>
              (parentXml \ "scm" \ "url").text
            }
          }
        }

        url.map(name -> _)

      .catchAll:
        _ =>
          // fall back to the usual
          ZIO.succeed:
            gav.artifactId.toString -> s"https://github.com/webjars/${gav.artifactId}"


  private def fetchVersions(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Scope, Throwable, MavenCentral.WithCacheInfo[Seq[MavenCentral.Version]]] =
    MavenCentral.searchVersions(groupId, artifactId).mapError {
      case e: MavenCentral.GroupIdOrArtifactIdNotFoundError => Throwable("groupId or artifactId not found")
      case t: Throwable => t
    }


  private def fetchArtifactIds(groupId: MavenCentral.GroupId): ZIO[Client & Scope, Throwable, MavenCentral.WithCacheInfo[Seq[MavenCentral.ArtifactId]]] =
    MavenCentral.searchArtifacts(groupId).mapBoth({
      case _: MavenCentral.GroupIdNotFoundError => Throwable("groupId not found")
      case t: Throwable => t
    }, {
      artifacts =>
        // workarounds
        artifacts.copy(value = artifacts.value.filterNot { artifactId =>
          artifactId.toString.startsWith("webjars-") ||
            artifactId.toString == "bower" ||
            artifactId.toString == "bowergithub" ||
            artifactId.toString == "2.11.2" // weird workaround for: https://repo1.maven.org/maven2/org/webjars/npm/2.11.2/
        })
    })


  private def refreshArtifact(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, versions: Seq[WebJarVersion], updateNumFiles: Boolean): ZIO[Client & Redis & Scope, Throwable, Unit] =
    logger.info(s"Refreshing artifact: $groupId:$artifactId")
    for
      versionsResult <- fetchVersions(groupId, artifactId)
      latestVersion = versionsResult.value.head
      gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, latestVersion)
      (name, url) <- fetchWebJarNameAndUrl(gav)
      _ <- WebJarsCache.setArtifactDetails(
        gav.noVersion,
        WebJarsCache.WebJarMeta(name, url, versionsResult.value.map(v => WebJarVersion(v.toString, None)).toList, versionsResult.maybeLastModified.getOrElse(ZonedDateTime.now()))
      )

      resolvedVersions = versions.flatMap: webJarVersion =>
        webJarVersion.numFiles.map(_ => webJarVersion.number)

      versionsToRefresh = versionsResult.value.diff(resolvedVersions)

      // don't block updating the artifact on filling in the version info
      // todo: we need to move this to another background process because we can get stuck with a WebJar that doesn't have all the numFiles info but is in the cache (ie process died mid-refresh)
      _ <-
        ZIO.when(updateNumFiles):
          ZIO.foreachDiscard(versionsToRefresh): version =>
            ZIO.fromFuture(ec => webJarsFileService.getNumFiles(gav.copy(version = version))).flatMap: numFiles =>
              WebJarsCache.updateVersion(gav.noVersion, version.toString, numFiles)
          .forkDaemon

    yield
      ()

  private[utils] def refreshGroup(groupId: MavenCentral.GroupId, updateNumFiles: Boolean = true): ZIO[Client & Redis, Throwable, Set[MavenCentral.ArtifactId]] =
    logger.info(s"Refreshing groupId: $groupId with limit ${maybeLimit.getOrElse("none")} and updateNumFiles: $updateNumFiles")

    ZIO.scoped:
      for
        artifactsResult <- fetchArtifactIds(groupId)

        artifactsWithLimit = maybeLimit.fold(artifactsResult.value)(artifactsResult.value.take)

        cachedArtifacts <- WebJarsCache.getArtifacts(groupId)

        missingArtifacts = artifactsWithLimit.diff(cachedArtifacts.keys.toSeq).toSet

        // first refresh any missing artifacts
        refreshMissing <- ZIO.foreach(missingArtifacts): artifactId =>
          refreshArtifact(groupId, artifactId, Seq.empty, updateNumFiles).as(artifactId)

        cachedArtifactsWithLimit = maybeLimit.fold(cachedArtifacts)(cachedArtifacts.take)

        // now go through each artifact and if it has been updated, refresh it
        refreshUpdated <- ZIO.filter(cachedArtifactsWithLimit.toSet): (artifactId, meta) =>
          MavenCentral.isModifiedSince(meta.lastModified, groupId, Some(artifactId)).orElseSucceed(false)
        .flatMap: artifacts =>
          ZIO.foreach(artifacts): (artifactId, meta) =>
            refreshArtifact(groupId, artifactId, meta.versions.toSeq, updateNumFiles).as(artifactId)
      yield
        refreshMissing ++ refreshUpdated

  def refreshAll(groupIds: Set[MavenCentral.GroupId]): ZIO[Client & Redis, Nothing, Unit] =
    ZIO.foreachDiscard(groupIds):
      groupId => refreshGroup(groupId).ignoreLogged

  if environment.mode != Mode.Test then
    refreshAll(allDeployables.groupIds()).repeat(Schedule.spaced(1.hour)).runToFuture(Client.default.orDie ++ valkey.layer)

  extension (m: Map[MavenCentral.ArtifactId, WebJarsCache.WebJarMeta])
    def toWebJars(groupId: MavenCentral.GroupId): Seq[WebJar] =
      m.map: (artifactId, meta) =>
        WebJar(groupId.toString, artifactId.toString, meta.name, meta.sourceUrl, meta.versions.toSeq)
      .toSeq

  override def featuredWebJars(groupId: MavenCentral.GroupId, limit: Int): Future[Seq[WebJar]] =
    WebJarsCache.getArtifacts(groupId, Some(limit)).map(_.toWebJars(groupId)).runToFuture(valkey.layer)

  override def searchWebJars(groupId: MavenCentral.GroupId, query: Option[String]): Future[Seq[WebJar]] =
    WebJarsCache.getArtifacts(groupId, maybeLimit, query).map(_.toWebJars(groupId)).runToFuture(valkey.layer)

}
