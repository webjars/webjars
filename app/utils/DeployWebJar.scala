package utils

import com.lumidion.sonatype.central.client.core.DeploymentState.{PUBLISHING, VALIDATED}
import com.lumidion.sonatype.central.client.core.{CheckStatusResponse, DeploymentId}
import io.lemonlabs.uri.AbsoluteUrl
import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream, ArchiveStreamFactory}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Source, SourceQueueWithComplete}
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.{Done, NotUsed}
import play.api.Configuration
import play.api.i18n.{Lang, Langs, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.concurrent.Futures
import utils.MavenCentral.{ArtifactId, GAV, GroupId}

import java.io.{BufferedInputStream, FileNotFoundException, InputStream}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Try, Using}


class DeployWebJar @Inject()(mavenCentral: MavenCentral, mavenCentralDeployer: MavenCentralDeployer, sourceLocator: SourceLocator, configuration: Configuration, heroku: Heroku)(implicit ec: ExecutionContext, futures: Futures, materializer: Materializer, actorSystem: ActorSystem) {

  val fork = configuration.getOptional[Boolean]("deploy.fork").getOrElse(false)

  def forkDeploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, force: Boolean): Source[String, Future[NotUsed]] = {
    val app = configuration.get[String]("deploy.herokuapp")
    val cmd = s"deploy ${deployable.name} $nameOrUrlish $upstreamVersion $deployDependencies $preventFork $force"
    heroku.dynoCreate(app, cmd, "Standard-2X")
  }

  def localDeploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, force: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[AbsoluteUrl] = None, maybeLicense: Option[String] = None): Source[String, Future[NotUsed]] = {

    def webJarNotYetDeployed(groupId: String, artifactId: String, version: String): Future[Unit] = {
      if (!force) {
        mavenCentral.fetchPom(GAV(groupId, artifactId, version), Some("https://oss.sonatype.org/content/repositories/releases")).flatMap { _ =>
          Future.failed(new IllegalStateException(s"WebJar $groupId $artifactId $version has already been deployed. Note that if you do not see it on webjars.org it can take >24 hours for the caches to update."))
        } recoverWith {
          case _: FileNotFoundException =>
            Future.unit
        }
      }
      else {
        Future.unit
      }
    }

    // todo: map on offer
    def doDeployDependencies(queue: SourceQueueWithComplete[String], packageInfo: PackageInfo): Future[Done] = {
      if (deployDependencies) {
        queue.offer("Determining dependency graph")

        deployable.depGraph(packageInfo).flatMap { depGraph =>

          val deployDepGraphMessage = if (depGraph.isEmpty) {
            "No dependencies."
          }
          else {
            "Deploying these dependencies:\n  " + depGraph.map(dep => dep._1 + "#" + dep._2).mkString("\n  ")
          }

          queue.offer(deployDepGraphMessage)

          def deployDep(deps: Map[String, String]): Future[Done] = {
            if (deps.isEmpty) {
              Future.successful(Done)
            }
            else {
              val (nameish, version) = deps.head
              queue.offer(s"Deploying Dependency: ${deployable.name} $nameish $version")
              deploy(deployable, nameish, version, false, false, force).runForeach(queue.offer).recover {
                // ignore failures
                case e =>
                  queue.offer(e.getMessage)
                  Done
              } flatMap { _ =>
                deployDep(deps.tail)
              }
            }
          }

          deployDep(depGraph)
        }
      }
      else {
        Future.successful(Done)
      }
    }

    Source.queue[String](Int.MaxValue, OverflowStrategy.backpressure).mapMaterializedValue { queue =>
      val future = for {
        packageInfo <- deployable.info(nameOrUrlish, upstreamVersion, maybeSourceUri)
        groupId = deployable.groupId
        artifactId <- deployable.artifactId(nameOrUrlish, upstreamVersion)

        releaseVersion = deployable.releaseVersion(maybeReleaseVersion, packageInfo)

        _ <- queue.offer(s"Got package info for $groupId $artifactId $releaseVersion")

        _ <- doDeployDependencies(queue, packageInfo)

        _ <- webJarNotYetDeployed(groupId, artifactId, releaseVersion)

        _ <- queue.offer(s"Resolving licenses & dependencies for $groupId $artifactId $releaseVersion")

        licenses <- maybeLicense.fold(deployable.licenses(nameOrUrlish, upstreamVersion, packageInfo)) { license =>
          Future.successful(Set(LicenseWithName(license)))
        }
        _ <- queue.offer(s"Resolved Licenses: ${licenses.mkString(",")}")

        mavenDependencies <- deployable.mavenDependencies(packageInfo.dependencies)
        _ <- queue.offer("Converted dependencies to Maven")

        optionalMavenDependencies <- deployable.mavenDependencies(packageInfo.optionalDependencies)
        _ <- queue.offer("Converted optional dependencies to Maven")

        sourceUrl <- sourceLocator.sourceUrl(packageInfo.sourceConnectionUri)
        _ <- queue.offer(s"Got the source URL: $sourceUrl")

        pom = templates.xml.pom(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses).toString()
        _ <- queue.offer("Generated POM")

        zip <- deployable.archive(nameOrUrlish, upstreamVersion)
        _ <- queue.offer(s"Fetched ${deployable.name} zip")

        excludes <- deployable.excludes(nameOrUrlish, upstreamVersion)

        pathPrefix <- deployable.pathPrefix(nameOrUrlish, releaseVersion, packageInfo)

        maybeBaseDirGlob <- deployable.maybeBaseDirGlob(nameOrUrlish)

        jar = WebJarCreator.createWebJar(zip, maybeBaseDirGlob, excludes, pom, packageInfo.name, licenses, groupId, artifactId, releaseVersion, pathPrefix)

        _ <- queue.offer(s"Created ${deployable.name} WebJar")

        gav = GAV(groupId, artifactId, releaseVersion)

        _ <- queue.offer(s"Creating Maven Central Release for $gav")

        (deploymentId, checker) <- mavenCentralDeployer.upload(gav, jar, pom).fold(Future.failed[(DeploymentId, () => Option[CheckStatusResponse])](new IllegalStateException(s"Could not upload $gav")))(Future.successful)

        _ <- queue.offer(s"Uploaded Maven Central Release for $gav")

        _ <- mavenCentralDeployer.waitForDeploymentState(VALIDATED, checker)

        _ <- queue.offer(s"Validated Maven Central Release for $gav")

        _ <- mavenCentralDeployer.publish(deploymentId).fold(Future.failed[Unit](new IllegalStateException(s"Could not publish $gav")))(Future.successful)

        _ <- mavenCentralDeployer.waitForDeploymentState(PUBLISHING, checker)

        _ <- queue.offer(s"""Deployed!
                          |It will take a few hours for the Maven Central index to update but you should be able to start using the ${deployable.name} WebJar shortly.
                          |GroupID = $groupId
                          |ArtifactID = $artifactId
                          |Version = $releaseVersion
            """.stripMargin)
      } yield NotUsed

      future.onComplete { t =>
        t.fold(queue.fail, _ => queue.complete())
      }

      future
    }
  }

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, force: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[AbsoluteUrl] = None, maybeLicense: Option[String] = None): Source[String, Future[NotUsed]] = {
    if (fork && !preventFork) {
      forkDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, true, force)
    }
    else {
      localDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, force, maybeReleaseVersion, maybeSourceUri, maybeLicense)
    }
  }

  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Set[License]], groupIdOverride: Option[String]): Future[(String, Array[Byte])] = {
    import deployable._

    for {
      packageInfo <- deployable.info(nameOrUrlish, upstreamVersion)
      groupId = groupIdOverride.getOrElse(deployable.groupId)
      artifactId <- deployable.artifactId(nameOrUrlish, upstreamVersion)

      releaseVersion = upstreamVersion.vless

      licenses <- licenseOverride.fold(licenses(nameOrUrlish, upstreamVersion, packageInfo))(Future.successful)

      mavenDependencies <- deployable.mavenDependencies(packageInfo.dependencies)

      optionalMavenDependencies <- deployable.mavenDependencies(packageInfo.optionalDependencies)

      sourceUrl <- sourceLocator.sourceUrl(packageInfo.sourceConnectionUri)

      pom = templates.xml.pom(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses).toString()

      zip <- deployable.archive(nameOrUrlish, upstreamVersion)

      excludes <- deployable.excludes(nameOrUrlish, upstreamVersion)

      pathPrefix <- deployable.pathPrefix(nameOrUrlish, releaseVersion, packageInfo)

      maybeBaseDirGlob <- deployable.maybeBaseDirGlob(nameOrUrlish)
    } yield artifactId -> WebJarCreator.createWebJar(zip, maybeBaseDirGlob, excludes, pom, packageInfo.name, licenses, groupId, artifactId, releaseVersion, pathPrefix)
  }

}

object DeployWebJar extends App {

  val (webJarType, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, force, maybeReleaseVersion, maybeSourceUri, maybeLicense) = if (args.length < 5) {
    val webJarType = StdIn.readLine("WebJar Type: ")
    val nameOrUrlish = StdIn.readLine("Name or URL: ")
    val upstreamVersion = StdIn.readLine("Upstream Version: ")
    val deployDependenciesIn = StdIn.readLine("Deploy dependencies (false|true): ")
    val preventForkIn = StdIn.readLine("Prevent Fork (false|true): ")
    val forceIn = StdIn.readLine("Force deploy (false|true): ")
    val releaseVersionIn = StdIn.readLine("Release Version (override): ")
    val sourceUriIn = StdIn.readLine("Source URI (override): ")
    val licenseIn = StdIn.readLine("License (override): ")

    val deployDependencies = if (deployDependenciesIn.isEmpty) false else deployDependenciesIn.toBoolean

    val preventFork = if (preventForkIn.isEmpty) false else preventForkIn.toBoolean

    val force = if (forceIn.isEmpty) false else forceIn.toBoolean

    val maybeReleaseVersion = if (releaseVersionIn.isEmpty) None else Some(releaseVersionIn)

    val maybeSourceUri = if (sourceUriIn.isEmpty) None else AbsoluteUrl.parseOption(sourceUriIn)

    val maybeLicense = if (licenseIn.isEmpty) None else Some(licenseIn)

    (webJarType, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, force, maybeReleaseVersion, maybeSourceUri, maybeLicense)
  }
  else {
    // todo: come up with a way to handle the optional params because if we fork then we lose them
    (args(0), args(1), args(2), args(3).toBoolean, args(4).toBoolean, args(5).toBoolean, None, None, None)
  }

  if (nameOrUrlish.isEmpty || upstreamVersion.isEmpty) {
    println("Name and version must be specified")
    sys.exit(1)
  }
  else {
    val app = new GuiceApplicationBuilder().build()

    implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
    implicit val materializer: Materializer = app.injector.instanceOf[Materializer]

    val deployWebJar = app.injector.instanceOf[DeployWebJar]

    val allDeployables = app.injector.instanceOf[AllDeployables]

    val deployFuture = allDeployables.fromName(webJarType).fold[Future[Done]] {
      Future.failed(new Exception(s"Specified WebJar type '$webJarType' can not be deployed"))
    } { deployable =>
      deployWebJar.deploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, force, maybeReleaseVersion, maybeSourceUri, maybeLicense).runForeach(println)
    }

    deployFuture.failed.foreach(e => println(e.getMessage))
    deployFuture.onComplete(_ => app.stop())
  }

}

trait Deployable {

  import Deployable._

  val licenseDetector: LicenseDetector
  val messages: MessagesApi
  val langs: Langs

  implicit val lang: Lang = langs.availables.head

  implicit class RichVersion(val s: Version) {
    def vless: Version = s.stripPrefix("v").replace("^v", "^").replace("~v", "v")

    def vwith: Version = if (s.startsWith("v")) s else "v" + s
  }

  val name: String

  val groupId: GroupId

  def artifactId(nameOrUrlish: NameOrUrlish, version: Version): Future[ArtifactId]

  def releaseVersion(maybeVersion: Option[Version], packageInfo: PackageInfo): String = maybeVersion.getOrElse(packageInfo.version).vless

  def excludes(nameOrUrlish: NameOrUrlish, version: Version): Future[Set[String]]

  val metadataFile: Option[String]

  def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): Future[Option[Version]]

  def pathPrefix(nameOrUrlish: NameOrUrlish, releaseVersion: Version, packageInfo: PackageInfo): Future[String]

  def info(nameOrUrlish: NameOrUrlish, version: Version, maybeSourceUri: Option[AbsoluteUrl] = None): Future[PackageInfo]

  def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]]

  def archive(nameOrUrlish: NameOrUrlish, version: Version): Future[InputStream]

  def file(nameOrUrlish: NameOrUrlish, version: Version, filename: String): Future[String]

  def versions(nameOrUrlish: NameOrUrlish): Future[Set[Version]]

  def licenses(nameOrUrlish: NameOrUrlish, version: Version, packageInfo: PackageInfo)(implicit ec: ExecutionContext): Future[Set[License]] = {

    def tryToGetLicenseFromVariousFiles(files: Set[String]): Future[License] = {
      files.headOption.fold[Future[License]](Future.failed(NoValidLicenses())) { licenseFile =>
        file(nameOrUrlish, version, licenseFile).flatMap(licenseDetector.licenseDetect).recoverWith {
          case _ => tryToGetLicenseFromVariousFiles(files.tail)
        }
      }
    }

    val normalizedLicenses = packageInfo.metadataLicenses.map { license =>
      val replacedDotSlash = if (license.startsWith("./")) {
        license.replace("./", "file://")
      }
      else {
        license
      }

      val replacedSeeLicenseIn = replacedDotSlash.replace("SEE LICENSE IN ", "file://")

      licenseReference(nameOrUrlish, version, replacedSeeLicenseIn)
    }

    val resolvedLicenses = Future.foldLeft(normalizedLicenses)(Set.empty[License])(_ ++ _)

    resolvedLicenses
      .filter(_.nonEmpty)
      .recoverWith {
        case _ =>
          tryToGetLicenseFromVariousFiles(licenseDetector.typicalLicenseFiles).map(Set(_))
      }
      .filter(_.nonEmpty)
      .recoverWith {
        case e: Exception =>
          val errorMessage = messages("licensenotfound", s"${this.name} - $nameOrUrlish $version")
          Future.failed(LicenseNotFoundException(errorMessage, e))
      }
  }

  def archiveFile[E <: ArchiveEntry](nameOrUrlish: NameOrUrlish, version: Version, filename: String)(implicit ec: ExecutionContext): Future[String] = {
    archive(nameOrUrlish, version).flatMap { resource =>
      Future.fromTry {
        Using(new BufferedInputStream(resource)) { inputStream =>
          val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ArchiveInputStream[E]](inputStream)
          val maybeEntry = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).find(_.getName == filename)
          maybeEntry.fold[Try[String]](Failure(new FileNotFoundException(s"Could not find $filename in archive for $nameOrUrlish $version"))) { _ =>
            Try {
              scala.io.Source.fromInputStream(archiveStream).mkString
            }
          }
        }.flatten
      }
    }
  }

  def licenseReference(nameOrUrlish: NameOrUrlish, version: Version, license: String)(implicit ec: ExecutionContext): Future[Set[License]] = {
    if (license.startsWith("http://") || license.startsWith("https://")) {
      // we need to fetch the file and try to detect the license from the contents
      licenseDetector.licenseDetect(AbsoluteUrl.parse(license)).map(Set(_))
    }
    else if (license.startsWith("file://")) {
      file(nameOrUrlish, version, license.stripPrefix("file://")).flatMap { file =>
        licenseDetector.licenseDetect(file)
      }.recover {
        case _ =>
          LicenseWithUrl(AbsoluteUrl.parse(license))
      }.map(Set(_))
    }
    else if (license.startsWith("(") && license.endsWith(")") && !license.contains("AND")) {
      // SPDX license expression
      Future.successful {
        license.stripPrefix("(").stripSuffix(")").replace(" or ", " OR ").split(" OR ").toSet.map(LicenseWithName)
      }
    }
    else {
      Future.successful(Set(LicenseWithName(license)))
    }
  }

  def parseDep(nameAndVersionish: (NameOrUrlish, Version)): (NameOrUrlish, Version) = {
    val (name, versionish) = nameAndVersionish

    if (versionish.contains("/")) {
      val urlish = versionish.takeWhile(_ != '#')
      val version = versionish.stripPrefix(urlish).stripPrefix("#").vless

      urlish -> version
    }
    else if (versionish.startsWith("npm:")) {
      val name = versionish.stripPrefix("npm:").takeWhile(_ != '@')
      val version = versionish.stripPrefix(name).dropWhile(_ != '@').stripPrefix("@")
      name -> version
    }
    else {
      name -> versionish.vless
    }
  }

  def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty[String, String])(implicit ec: ExecutionContext, futures: Futures): Future[Map[String, String]]

}

object Deployable {
  type NameOrUrlish = String
  type Version = String
}

class AllDeployables @Inject() (classic: Classic, npm: NPM) {

  def fromGroupId(groupId: String): Option[Deployable] =
    if (groupId == classic.groupId)
      Some(classic)
    else if (groupId == npm.groupId)
      Some(npm)
    else
      None

  def groupIds(): Set[String] = Set(classic.groupId, npm.groupId)

  def fromName(name: String): Option[Deployable] =
    if (name.equalsIgnoreCase(classic.name))
      Some(classic)
    else if (name.equalsIgnoreCase(npm.name))
      Some(npm)
    else
      None

}
