package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import io.lemonlabs.uri.AbsoluteUrl
import webjars.config.AppConfig
import webjars.utils.Deployable.NameOrUrlish
import zio.*
import zio.direct.*
import zio.stream.ZStream

import java.io.FileNotFoundException

trait DeployWebJar:
  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, force: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[AbsoluteUrl] = None, maybeLicense: Option[String] = None): ZStream[Scope, Throwable, String]
  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Array[Byte])]

case class DeployWebJarLive(mavenCentralWebJars: MavenCentralWebJars, mavenCentralDeployer: MavenCentralDeployer, sourceLocator: SourceLocator, config: AppConfig, heroku: Heroku) extends DeployWebJar:

  private val fork: Boolean = config.deployFork

  private def forkDeploy(deployable: Deployable, nameOrUrlish: NameOrUrlish, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, force: Boolean): ZStream[Scope, Throwable, String] =
    val app = config.deployHerokuApp
    val cmd = s"deploy ${deployable.name} $nameOrUrlish $upstreamVersion $deployDependencies $preventFork $force"
    heroku.dynoCreate(app, cmd, "Standard-2X")

  private def localDeploy(deployable: Deployable, nameOrUrlish: NameOrUrlish, upstreamVersion: String, deployDependencies: Boolean, force: Boolean, maybeReleaseVersion: Option[String], maybeSourceUri: Option[AbsoluteUrl], maybeLicense: Option[String]): ZStream[Scope, Throwable, String] =

    def webJarNotYetDeployed(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Scope, Throwable, Unit] =
      if !force then
        mavenCentralWebJars.fetchPom(MavenCentral.GroupArtifactVersion(groupId, artifactId, version)).flatMap { _ =>
          ZIO.fail(new IllegalStateException(s"WebJar $groupId $artifactId $version has already been deployed. Note that if you do not see it on webjars.org it can take >24 hours for the caches to update."))
        }.catchSome {
          case _: FileNotFoundException => ZIO.unit
        }
      else
        ZIO.unit

    def doDeployDependencies(packageInfo: PackageInfo): ZStream[Scope, Throwable, String] =
      if deployDependencies then
        ZStream.fromZIO(ZIO.succeed("Determining dependency graph")) ++
        ZStream.unwrap {
          deployable.depGraph(packageInfo).map { depGraph =>
            val deployDepGraphMessage = if depGraph.isEmpty then
              "No dependencies."
            else
              "Deploying these dependencies:\n  " + depGraph.map(dep => dep._1 + "#" + dep._2).mkString("\n  ")

            ZStream.succeed(deployDepGraphMessage) ++
            ZStream.fromIterable(depGraph.toSeq).flatMap { case (nameish, version) =>
              ZStream.succeed(s"Deploying Dependency: ${deployable.name} $nameish $version") ++
              deploy(deployable, nameish, version, false, false, force).catchAll { e =>
                ZStream.succeed(e.getMessage)
              }
            }
          }
        }
      else
        ZStream.empty

    ZStream.unwrap:
      defer:
        val packageInfo = deployable.info(nameOrUrlish, upstreamVersion, maybeSourceUri).run
        val groupId = deployable.groupId
        val artifactId = deployable.artifactId(nameOrUrlish).run
        val releaseVersion = deployable.releaseVersion(maybeReleaseVersion, packageInfo)

        ZStream.succeed(s"Got package info for $groupId $artifactId $releaseVersion") ++
        doDeployDependencies(packageInfo) ++
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
                   |It will take a few hours for the Maven Central index to update but you should be able to start using the ${deployable.name} WebJar shortly.
                   |GroupID = $groupId
                   |ArtifactID = $artifactId
                   |Version = $releaseVersion
                   """.stripMargin

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, force: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[AbsoluteUrl] = None, maybeLicense: Option[String] = None): ZStream[Scope, Throwable, String] =
    if fork && !preventFork then
      forkDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, true, force)
    else
      localDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, force, maybeReleaseVersion, maybeSourceUri, maybeLicense)

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
  val live: ZLayer[MavenCentralWebJars & MavenCentralDeployer & SourceLocator & AppConfig & Heroku, Nothing, DeployWebJar] = ZLayer.derive[DeployWebJarLive]
