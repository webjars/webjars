package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.{Client, URL}
import zio.redis.Redis
import zio.stream.ZStream

import java.io.FileNotFoundException

trait DeployWebJar[Env]:
  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope & Client & Redis & Env, Throwable, String]
  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Array[Byte])]

case class DeployWebJarLive[Env](mavenCentralWebJars: MavenCentralWebJars, mavenCentralDeployer: MavenCentralDeployer[Env], sourceLocator: SourceLocator) extends DeployWebJar[Env]:

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope & Client & Redis & Env, Throwable, String] =

    def webJarNotYetDeployed(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Scope & Redis & Client, Throwable, Unit] =
      mavenCentralWebJars.fetchPom(MavenCentral.GroupArtifactVersion(groupId, artifactId, version)).flatMap { _ =>
        // Already on Maven Central. Queue it for cache refresh (the ZSET
        // add is idempotent — duplicates are harmless) and fail the deploy
        // with a clearer next-step message.
        WebJarsCache.addPendingDeploy(WebJarsCache.PendingDeploy(groupId, artifactId, version)).ignoreLogged *>
          ZIO.fail(new IllegalStateException(
            s"WebJar $groupId $artifactId $version has already been deployed to Maven Central. " +
              "Queued for cache refresh — should appear on webjars.org within ~1 hour."
          ))
      }.catchSome {
        case _: FileNotFoundException => ZIO.unit
      }

    ZStream.unwrap:
      defer:
        val packageInfo = deployable.info(nameOrUrlish, upstreamVersion, maybeSourceUri).run
        val groupId = deployable.groupId
        val artifactId = deployable.artifactId(nameOrUrlish).run
        val releaseVersion = deployable.releaseVersion(maybeReleaseVersion, packageInfo)

        ZStream.succeed(s"Got package info for $groupId $artifactId $releaseVersion") ++
        ZStream.unwrap:
          defer:
            webJarNotYetDeployed(groupId, artifactId, releaseVersion).run

            val licenses = maybeLicense.fold(deployable.licenses(nameOrUrlish, upstreamVersion, packageInfo))(license =>
              ZIO.succeed(Set[License](LicenseWithName(license)))
            ).run

            val mavenDependencies = deployable.mavenDependencies(packageInfo.dependencies).run
            val optionalMavenDependencies = deployable.mavenDependencies(packageInfo.optionalDependencies).run
            val sourceUrl = sourceLocator.sourceUrl(packageInfo.sourceConnectionUri).run
            val pom = PomTemplate(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses)
            val zip = deployable.archive(nameOrUrlish, upstreamVersion).run
            val excludes = deployable.excludes(nameOrUrlish).run
            val pathPrefix = deployable.pathPrefix(artifactId, releaseVersion, packageInfo)
            val maybeBaseDirGlob = deployable.maybeBaseDirGlob(nameOrUrlish).run
            val jar = WebJarCreator.createWebJar(ZStream.fromInputStream(zip), maybeBaseDirGlob, excludes, pom, packageInfo.name, licenses, groupId, artifactId, releaseVersion, pathPrefix).run
            val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, releaseVersion)

            ZStream(
              s"Resolving licenses & dependencies for $groupId $artifactId $releaseVersion",
              s"Resolved Licenses: ${licenses.mkString(",")}",
              "Converted dependencies to Maven",
              "Converted optional dependencies to Maven",
              s"Got the source URL: $sourceUrl",
              "Generated POM",
              s"Fetched ${deployable.name} zip",
              s"Created ${deployable.name} WebJar",
              s"Deploying Maven Central Release for $gav",
            ) ++ ZStream.fromZIO:
              mavenCentralDeployer.publish(gav, jar, pom)
                // Record the GAV in the pending-deploys queue so the next
                // refresh cycle picks up the new version once MC propagates.
                // .ignoreLogged so a Valkey hiccup never blocks a successful
                // publish — refresh-from-cache is best-effort.
                .zipLeft(WebJarsCache.addPendingDeploy(WebJarsCache.PendingDeploy(groupId, artifactId, releaseVersion)).ignoreLogged)
                .as:
                  s"""Deployed!
                     |It can take an hour or more for the artifact to be available in Maven Central.
                     |GroupID = $groupId
                     |ArtifactID = $artifactId
                     |Version = $releaseVersion
                     """.stripMargin

  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Array[Byte])] =
    import deployable.*
    defer:
      val packageInfo = deployable.info(nameOrUrlish, upstreamVersion).run
      val groupId = groupIdOverride.getOrElse(deployable.groupId)
      val artifactId = deployable.artifactId(nameOrUrlish).run
      val releaseVersion = MavenCentral.Version(upstreamVersion.vless)
      val licenses = licenseOverride.fold(deployable.licenses(nameOrUrlish, upstreamVersion, packageInfo))(ZIO.succeed(_)).run
      val mavenDependencies = deployable.mavenDependencies(packageInfo.dependencies).run
      val optionalMavenDependencies = deployable.mavenDependencies(packageInfo.optionalDependencies).run
      val sourceUrl = sourceLocator.sourceUrl(packageInfo.sourceConnectionUri).run
      val pom = PomTemplate(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses)
      val zip = deployable.archive(nameOrUrlish, upstreamVersion).run
      val excludes = deployable.excludes(nameOrUrlish).run
      val pathPrefix = deployable.pathPrefix(artifactId, releaseVersion, packageInfo)
      val maybeBaseDirGlob = deployable.maybeBaseDirGlob(nameOrUrlish).run
      val jar = WebJarCreator.createWebJar(ZStream.fromInputStream(zip), maybeBaseDirGlob, excludes, pom, packageInfo.name, licenses, groupId, artifactId, releaseVersion, pathPrefix).run
      ZIO.succeed(artifactId -> jar).run

object DeployWebJar:
  def live[Env : Tag]: ZLayer[MavenCentralWebJars & MavenCentralDeployer[Env] & SourceLocator, Nothing, DeployWebJar[Env]] =
    ZLayer.derive[DeployWebJarLive[Env]]
