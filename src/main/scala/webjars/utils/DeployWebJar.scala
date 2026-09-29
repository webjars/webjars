package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.MavenCentralRepo
import zio.*
import zio.direct.*
import zio.http.{Client, URL}
import zio.redis.Redis
import zio.stream.ZStream

import java.io.FileNotFoundException

final case class DeploymentPreflight(packageInfo: PackageInfo, licenses: Set[License], alreadyDeployed: Boolean = false)

trait DeployWebJar[Env]:
  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope & Client & Redis & MavenCentralRepo & Env, Throwable, String]

  def preflight(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String): ZIO[Scope & Redis & MavenCentralRepo, Throwable, DeploymentPreflight] =
    deployable.info(nameOrUrlish, upstreamVersion).flatMap { packageInfo =>
      deployable.licenses(nameOrUrlish, upstreamVersion, packageInfo).map(DeploymentPreflight(packageInfo, _))
    }

  def deployPreflighted(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, preflight: DeploymentPreflight): ZStream[Scope & Client & Redis & MavenCentralRepo & Env, Throwable, String] =
    deploy(deployable, nameOrUrlish, upstreamVersion)

  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Deployable.ArchiveStream)]

case class DeployWebJarLive[Env](mavenCentralWebJars: MavenCentralWebJars, mavenCentralDeployer: MavenCentralDeployer[Env], sourceLocator: SourceLocator) extends DeployWebJar[Env]:

  private def webJarAlreadyDeployed(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Scope & Redis & MavenCentralRepo, Throwable, Boolean] =
    mavenCentralWebJars.fetchPom(MavenCentral.GroupArtifactVersion(groupId, artifactId, version)).flatMap { _ =>
      WebJarsCache.addPendingDeploy(WebJarsCache.PendingDeploy(groupId, artifactId, version)).ignoreLogged.as(true)
    }.catchSome {
      case _: FileNotFoundException => ZIO.succeed(false)
    }

  override def preflight(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String): ZIO[Scope & Redis & MavenCentralRepo, Throwable, DeploymentPreflight] =
    defer:
      val packageInfo = deployable.info(nameOrUrlish, upstreamVersion).run
      val artifactId = deployable.artifactId(nameOrUrlish).run
      val releaseVersion = deployable.releaseVersion(None, packageInfo)
      val alreadyDeployed = webJarAlreadyDeployed(deployable.groupId, artifactId, releaseVersion).run
      val licenses =
        if alreadyDeployed then Set.empty[License]
        else deployable.licenses(nameOrUrlish, upstreamVersion, packageInfo).run
      DeploymentPreflight(packageInfo, licenses, alreadyDeployed)

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URL] = None, maybeLicense: Option[String] = None): ZStream[Scope & Client & Redis & MavenCentralRepo & Env, Throwable, String] =
    deployWith(deployable, nameOrUrlish, upstreamVersion, maybeReleaseVersion, maybeSourceUri, maybeLicense, None)

  override def deployPreflighted(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, preflight: DeploymentPreflight): ZStream[Scope & Client & Redis & MavenCentralRepo & Env, Throwable, String] =
    deployWith(deployable, nameOrUrlish, upstreamVersion, None, None, None, Some(preflight))

  private def deployWith(
    deployable: Deployable,
    nameOrUrlish: String,
    upstreamVersion: String,
    maybeReleaseVersion: Option[String],
    maybeSourceUri: Option[URL],
    maybeLicense: Option[String],
    maybePreflight: Option[DeploymentPreflight],
  ): ZStream[Scope & Client & Redis & MavenCentralRepo & Env, Throwable, String] =

    ZStream.unwrap:
      defer:
        val packageInfo = maybePreflight.fold(
          deployable.info(nameOrUrlish, upstreamVersion, maybeSourceUri)
        )(preflight => ZIO.succeed(preflight.packageInfo)).run
        val groupId = deployable.groupId
        val artifactId = deployable.artifactId(nameOrUrlish).run
        val releaseVersion = deployable.releaseVersion(maybeReleaseVersion, packageInfo)
        val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, releaseVersion)

        // Each stage announces itself BEFORE running its work, so a
        // partial deploy log on failure pinpoints which stage was in
        // flight. Earlier this whole flow was one big `defer` block whose
        // status messages were emitted only at the end — meaning a
        // failure mid-flow (e.g. `LicenseNotFoundException` during
        // `deployable.licenses`) showed only the bare exception message
        // with no context for which stage produced it. See issue #2229.
        val licensesEffect = maybeLicense.fold(
          maybePreflight.fold(
            deployable.licenses(nameOrUrlish, upstreamVersion, packageInfo)
          )(preflight => ZIO.succeed(preflight.licenses))
        )(license => ZIO.succeed(Set[License](LicenseWithName(license))))

        val alreadyDeployedEffect =
          if maybePreflight.exists(_.alreadyDeployed) then ZIO.succeed(true)
          else webJarAlreadyDeployed(groupId, artifactId, releaseVersion)

        ZStream.succeed(s"Got package info for $groupId $artifactId $releaseVersion") ++
        ZStream.succeed(s"Verifying $gav is not already on Maven Central") ++
        ZStream.fromZIO(alreadyDeployedEffect).flatMap { alreadyDeployed =>
          if alreadyDeployed then
            ZStream.succeed(
              s"WebJar $groupId $artifactId $releaseVersion has already been deployed to Maven Central. " +
                "Queued for cache refresh — should appear on webjars.org within ~1 hour."
            )
          else
            ZStream.succeed(s"Resolving licenses for $gav") ++
            ZStream.fromZIO(licensesEffect).flatMap { licenses =>
          ZStream.succeed(s"Resolved Licenses: ${licenses.mkString(",")}") ++
          ZStream.succeed(s"Resolving Maven dependencies for $gav") ++
          ZStream.fromZIO(deployable.mavenDependencies(packageInfo.dependencies)).flatMap { mavenDependencies =>
            ZStream.fromZIO(deployable.mavenDependencies(packageInfo.optionalDependencies)).flatMap { optionalMavenDependencies =>
              ZStream.succeed("Converted dependencies to Maven") ++
              ZStream.succeed("Converted optional dependencies to Maven") ++
              ZStream.succeed(s"Locating source URL for $gav") ++
              ZStream.fromZIO(sourceLocator.sourceUrl(packageInfo.sourceConnectionUri)).flatMap { sourceUrl =>
                val pom = PomTemplate(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses)
                val pathPrefix = deployable.pathPrefix(artifactId, releaseVersion, packageInfo)
                ZStream.succeed(s"Got the source URL: $sourceUrl") ++
                ZStream.succeed("Generated POM") ++
                ZStream.succeed(s"Fetching ${deployable.name} archive for $gav") ++
                ZStream.succeed(deployable.archive(nameOrUrlish, upstreamVersion)).flatMap { archive =>
                  ZStream.fromZIO(deployable.excludes(nameOrUrlish)).flatMap { excludes =>
                    ZStream.fromZIO(deployable.maybeBaseDirGlob(nameOrUrlish)).flatMap { maybeBaseDirGlob =>
                      val jar = WebJarCreator.createWebJar(
                        archive, maybeBaseDirGlob, excludes, pom,
                        packageInfo.name, licenses, groupId, artifactId, releaseVersion, pathPrefix,
                      )
                      ZStream.succeed(s"Streaming ${deployable.name} WebJar to Maven Central for $gav") ++
                      ZStream.fromZIO {
                        GavPublicationGuard.publish(
                          gav,
                          webJarAlreadyDeployed(groupId, artifactId, releaseVersion),
                        )(
                          mavenCentralDeployer.publish(gav, jar, pom)
                        )
                          // Record the GAV in the pending-deploys queue so the next
                          // refresh cycle picks up the new version once MC propagates.
                          // .ignoreLogged so a Valkey hiccup never blocks a successful
                          // publish — refresh-from-cache is best-effort.
                          .zipLeft(WebJarsCache.addPendingDeploy(WebJarsCache.PendingDeploy(groupId, artifactId, releaseVersion)).ignoreLogged)
                          .map:
                            case GavPublicationGuard.Outcome.Published =>
                              s"""Deployed!
                                 |It can take an hour or more for the artifact to be available in Maven Central.
                                 |GroupID = $groupId
                                 |ArtifactID = $artifactId
                                 |Version = $releaseVersion
                                 |""".stripMargin
                            case GavPublicationGuard.Outcome.AlreadyPublished | GavPublicationGuard.Outcome.Joined =>
                              s"WebJar $gav was published by another deployment. Queued for cache refresh — should appear on webjars.org within ~1 hour."
                      }
                    }
                  }
                }
              }
            }
          }
        }
        }

  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[MavenCentral.GroupId]): ZIO[Scope, Throwable, (MavenCentral.ArtifactId, Deployable.ArchiveStream)] =
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
      val archive = deployable.archive(nameOrUrlish, upstreamVersion)
      val excludes = deployable.excludes(nameOrUrlish).run
      val pathPrefix = deployable.pathPrefix(artifactId, releaseVersion, packageInfo)
      val maybeBaseDirGlob = deployable.maybeBaseDirGlob(nameOrUrlish).run
      val jar = WebJarCreator.createWebJar(archive, maybeBaseDirGlob, excludes, pom, packageInfo.name, licenses, groupId, artifactId, releaseVersion, pathPrefix)
      artifactId -> jar

object DeployWebJar:
  def live[Env : Tag]: ZLayer[MavenCentralWebJars & MavenCentralDeployer[Env] & SourceLocator, Nothing, DeployWebJar[Env]] =
    ZLayer.derive[DeployWebJarLive[Env]]
