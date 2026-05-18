package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.URL
import zio.stream.ZStream

import java.io.FileNotFoundException

trait DeployWebJar:
  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope, Throwable, String]
  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Array[Byte])]

case class DeployWebJarLive(mavenCentralWebJars: MavenCentralWebJars, mavenCentralDeployer: MavenCentralDeployer, sourceLocator: SourceLocator) extends DeployWebJar:

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope, Throwable, String] =

    def webJarNotYetDeployed(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Scope, Throwable, Unit] =
      mavenCentralWebJars.fetchPom(MavenCentral.GroupArtifactVersion(groupId, artifactId, version)).flatMap { _ =>
        ZIO.fail(new IllegalStateException(s"WebJar $groupId $artifactId $version has already been deployed. Note that if you do not see it on webjars.org it can take >24 hours for the caches to update."))
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
              mavenCentralDeployer.publish(gav, jar, pom).as:
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
  val live: ZLayer[MavenCentralWebJars & MavenCentralDeployer & SourceLocator, Nothing, DeployWebJar] = ZLayer.derive[DeployWebJarLive]
