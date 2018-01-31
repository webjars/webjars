package utils

import java.io.{FileNotFoundException, InputStream}
import java.net.URI
import javax.inject.Inject

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import models.WebJarType
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.concurrent.Futures
import play.api.libs.json.JsValue
import play.api.Configuration

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.Try


class DeployWebJar @Inject()(git: Git, binTray: BinTray, maven: Maven, mavenCentral: MavenCentral, licenseDetector: LicenseDetector, sourceLocator: SourceLocator, configuration: Configuration, heroku: Heroku)(implicit ec: ExecutionContext, futures: Futures, materializer: Materializer, actorSystem: ActorSystem) {

  val fork = configuration.getOptional[Boolean]("deploy.fork").getOrElse(false)
  val binTraySubject = "webjars"
  val binTrayRepo = "maven"

  def forkDeploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean)(attach: Boolean, preventFork: Boolean): Source[String, Future[Option[JsValue]]] = {
    val app = configuration.get[String]("deploy.herokuapp")
    val cmd = s"deploy ${WebJarType.toString(deployable)} $nameOrUrlish $upstreamVersion $deployDependencies $preventFork"
    heroku.dynoCreate(app, attach, cmd, "Standard-2X")
  }

  def localDeploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URI] = None, maybeLicense: Option[String] = None)(preventFork: Boolean): Source[String, Future[Option[JsValue]]] = {
    def licenses(packageInfo: PackageInfo, version: String): Future[Set[String]] = {
      maybeLicense.fold {
        licenseDetector.resolveLicenses(deployable, packageInfo, Some(version))
      } { license =>
        Future.successful(license.split(",").toSet)
      }
    }

    def webJarNotYetDeployed(groupId: String, artifactId: String, version: String): Future[Unit] = {
      mavenCentral.fetchPom(groupId, artifactId, version, Some("https://oss.sonatype.org/content/repositories/releases")).flatMap { elem =>
        Future.failed(new IllegalStateException(s"WebJar $groupId $artifactId $version has already been deployed"))
      } recoverWith {
        case _: FileNotFoundException =>
          Future.successful(Unit)
      }
    }

    Source.queue[String](256, OverflowStrategy.fail).mapMaterializedValue { queue =>

      def doDeployDependencies(packageInfo: PackageInfo): Source[String, Future[Seq[JsValue]]] = {
        def emptySource = Source.empty[String].mapMaterializedValue(_ => Future.successful(Seq.empty[JsValue]))

        if (deployDependencies) {
          val deployFuture = for {
            _ <- queue.offer("Determining dependency graph")

            depGraph <- deployable.depGraph(packageInfo)

            deployDepGraphMessage = if (depGraph.isEmpty) {
              "No dependencies. We are done here!"
            }
            else {
              "Deploying these dependencies: " + depGraph.map(dep => dep._1 + "#" + dep._2).mkString(",")
            }
            _ <- queue.offer(deployDepGraphMessage)
          } yield depGraph.map { case (nameish, version) =>
            deploy(deployable, nameish, version, false)(false, preventFork)
          }.foldLeft(emptySource) { (s1, s2) =>
            s1.mergeMat(s2) { (fm1, fm2) =>
              for {
                m1 <- fm1
                m2 <- fm2
              } yield m1 ++ m2
            }
          }

          Source.fromFutureSource(deployFuture).mapMaterializedValue(_.flatten)
        }
        else {
          emptySource
        }
      }

      val deployFuture = for {

        packageInfo <- deployable.info(nameOrUrlish, Some(upstreamVersion), maybeSourceUri)
        groupId <- deployable.groupId(nameOrUrlish)
        artifactId <- deployable.artifactId(nameOrUrlish)
        mavenBaseDir = groupId.replaceAllLiterally(".", "/")

        releaseVersion = deployable.releaseVersion(maybeReleaseVersion, packageInfo)

        _ <- queue.offer(s"Got package info for $groupId $artifactId $releaseVersion")

        _ <- doDeployDependencies(packageInfo).to(Sink.foreach(queue.offer)).run().recover { case _ => () } // ignore failures

        _ <- webJarNotYetDeployed(groupId, artifactId, releaseVersion)

        licenses <- licenses(packageInfo, upstreamVersion)

        _ <- queue.offer(s"Resolved Licenses: $licenses")
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

        jar = WebJarCreator.createWebJar(zip, deployable.contentsInSubdir, excludes, pom, groupId, artifactId, releaseVersion, pathPrefix)

        _ <- queue.offer(s"Created ${deployable.name} WebJar")

        packageName = s"$groupId:$artifactId"

        createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $artifactId", Seq("webjar", artifactId), licenses, packageInfo.sourceConnectionUri, packageInfo.maybeHomepageUrl, packageInfo.maybeIssuesUrl, packageInfo.maybeGitHubOrgRepo)
        _ <- queue.offer("Created BinTray Package")

        createVersion <- binTray.createOrOverwriteVersion(binTraySubject, binTrayRepo, packageName, releaseVersion, s"$artifactId WebJar release $releaseVersion", Some(s"v$releaseVersion"))
        _ <- queue.offer("Created BinTray Version")
        publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion.pom", pom.getBytes)
        publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion.jar", jar)
        emptyJar = WebJarCreator.emptyJar()
        publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion-sources.jar", emptyJar)
        publishJavadocJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion-javadoc.jar", emptyJar)
        _ <- queue.offer("Published BinTray Assets")
        signVersion <- binTray.signVersion(binTraySubject, binTrayRepo, packageName, releaseVersion)
        _ <- queue.offer("Signed BinTray Assets")
        publishVersion <- binTray.publishVersion(binTraySubject, binTrayRepo, packageName, releaseVersion)
        _ <- queue.offer("Published BinTray Version")

        syncToMavenCentral <- binTray.syncToMavenCentral(binTraySubject, binTrayRepo, packageName, releaseVersion)
        _ <- queue.offer("Synced With Maven Central")
        _ <- queue.offer(s"""Deployed!
             |It will take a few hours for the Maven Central index to update but you should be able to start using the ${deployable.name} WebJar shortly.
             |GroupID = $groupId
             |ArtifactID = $artifactId
             |Version = $releaseVersion
            """.stripMargin)
        _ = queue.complete()
      } yield None

      deployFuture.failed.foreach { e =>
        queue.offer(e.getMessage).foreach(_ => queue.complete())
      }

      deployFuture
    }
  }

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URI] = None, maybeLicense: Option[String] = None)(attach: Boolean, preventFork: Boolean): Source[String, Future[Option[JsValue]]] = {
    if (fork && !preventFork) {
      forkDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies)(attach, true)
    }
    else {
      localDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, maybeReleaseVersion, maybeSourceUri, maybeLicense)(preventFork)
    }
  }

}

object DeployWebJar extends App {

  val (webJarType, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, maybeReleaseVersion, maybeSourceUri, maybeLicense) = if (args.length < 5) {
    val webJarType = StdIn.readLine("WebJar Type: ")
    val nameOrUrlish = StdIn.readLine("Name or URL: ")
    val upstreamVersion = StdIn.readLine("Upstream Version: ")
    val deployDependenciesIn = StdIn.readLine("Deploy dependencies (true|false): ")
    val preventForkIn = StdIn.readLine("Prevent Fork (true|false): ")
    val releaseVersionIn = StdIn.readLine("Release Version (override): ")
    val sourceUriIn = StdIn.readLine("Source URI (override): ")
    val licenseIn = StdIn.readLine("License (override): ")

    val deployDependencies = if (deployDependenciesIn.isEmpty) false else deployDependenciesIn.toBoolean

    val preventFork = if (preventForkIn.isEmpty) false else preventForkIn.toBoolean

    val maybeReleaseVersion = if (releaseVersionIn.isEmpty) None else Some(releaseVersionIn)

    val maybeSourceUri = if (sourceUriIn.isEmpty) None else Try(new URI(sourceUriIn)).toOption

    val maybeLicense = if (licenseIn.isEmpty) None else Some(licenseIn)

    (webJarType, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, maybeReleaseVersion, maybeSourceUri, maybeLicense)
  }
  else {
    (args(0), args(1), args(2), args(3).toBoolean, args(4).toBoolean, None, None, None)
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

    val npm = app.injector.instanceOf[NPM]
    val bower = app.injector.instanceOf[Bower]
    val bowerGitHub = app.injector.instanceOf[BowerGitHub]

    val allDeployables = Set(npm, bower, bowerGitHub)

    val deployFuture = WebJarType.fromString(webJarType, allDeployables).fold[Future[_]] {
      Future.failed(new Exception(s"Specified WebJar type '$webJarType' can not be deployed"))
    } { deployable =>
      val source = deployWebJar.deploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, maybeReleaseVersion, maybeSourceUri, maybeLicense)(true, preventFork)
      source.runForeach(println)
    }

    deployFuture.failed.foreach(e => println(e.getMessage))
    deployFuture.onComplete(_ => app.stop())
  }

}

trait Deployable extends WebJarType {

  implicit class RichString(val s: String) {
    def vless: String = s.stripPrefix("v").replaceAllLiterally("^v", "^").replaceAllLiterally("~v", "v")
  }

  def groupId(nameOrUrlish: String): Future[String]
  def artifactId(nameOrUrlish: String): Future[String]
  def releaseVersion(maybeVersion: Option[String], packageInfo: PackageInfo): String = maybeVersion.getOrElse(packageInfo.version).stripPrefix("v")
  def excludes(nameOrUrlish: String, version: String): Future[Set[String]]
  val metadataFile: String
  val contentsInSubdir: Boolean
  def pathPrefix(nameOrUrlish: String, releaseVersion: String, packageInfo: PackageInfo): Future[String]
  def info(nameOrUrlish: String, maybeVersion: Option[String], maybeSourceUri: Option[URI] = None): Future[PackageInfo]
  def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]]
  def archive(nameOrUrlish: String, version: String): Future[InputStream]
  def versions(nameOrUrlish: String): Future[Set[String]]

  def parseDep(nameAndVersionish: (String, String)): (String, String) = {
    val (name, versionish) = nameAndVersionish

    if (versionish.contains("/")) {
      val urlish = versionish.takeWhile(_ != '#')
      val version = versionish.stripPrefix(urlish).stripPrefix("#").vless

      urlish -> version
    }
    else {
      name -> versionish.vless
    }
  }

  def latestDep(nameOrUrlish: String, version: String)(implicit ec: ExecutionContext): Future[String] = {
    versions(nameOrUrlish).flatMap { availableVersions =>
      val versionRange = SemVer.parseSemVerRange(version)
      SemVer.latestInRange(versionRange, availableVersions).fold {
        Future.failed[String](new Exception("Could not find a valid version in the provided range"))
      } { version =>
        Future.successful(version)
      }
    }
  }

  def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty[String, String])(implicit ec: ExecutionContext, futures: Futures): Future[Map[String, String]] = {
    import play.api.libs.concurrent.Futures._

    import scala.concurrent.duration._

    def depResolver(unresolvedDeps: Map[String, String], resolvedDeps: Map[String, String]): Future[(Map[String, String], Map[String, String])] = {

      val packagesToResolve = unresolvedDeps.filterKeys(!resolvedDeps.contains(_))

      packagesToResolve.headOption.fold {
        Future.successful(packagesToResolve -> resolvedDeps)
      } { dep =>
        val (nameOrUrlish, versionish) = parseDep(dep)
        latestDep(nameOrUrlish, versionish).flatMap { version =>
          val newResolvedDeps = resolvedDeps + (nameOrUrlish -> version)
          info(nameOrUrlish, Some(version)).flatMap { newPackageInfo =>
            val newUnresolvedDeps = packagesToResolve.tail ++ newPackageInfo.dependencies.map(parseDep)

            depResolver(newUnresolvedDeps, newResolvedDeps)
          }
        } recoverWith {
          // just skip deps that can't be resolved
          case _ => depResolver(packagesToResolve.tail, resolvedDeps)
        }
      }
    }

    depResolver(packageInfo.dependencies.map(parseDep), Map.empty[String, String]).map(_._2).withTimeout(10.minutes)
  }

}
