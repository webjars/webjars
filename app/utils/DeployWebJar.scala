package utils

import java.io.{FileNotFoundException, InputStream}
import java.net.URI
import javax.inject.Inject

import akka.actor.{ActorSystem, Props}
import akka.stream.Materializer
import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.Request
import akka.stream.scaladsl.Source
import akka.{Done, NotUsed}
import models.WebJarType
import play.api.Configuration
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.concurrent.Futures

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success, Try}


class DeployWebJar @Inject()(git: Git, binTray: BinTray, maven: Maven, mavenCentral: MavenCentral, licenseDetector: LicenseDetector, sourceLocator: SourceLocator, configuration: Configuration, heroku: Heroku)(implicit ec: ExecutionContext, futures: Futures, materializer: Materializer, actorSystem: ActorSystem) {

  val fork = configuration.getOptional[Boolean]("deploy.fork").getOrElse(false)
  val binTraySubject = "webjars"
  val binTrayRepo = "maven"

  def forkDeploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean): Source[String, Future[NotUsed]] = {
    val app = configuration.get[String]("deploy.herokuapp")
    val cmd = s"deploy ${WebJarType.toString(deployable)} $nameOrUrlish $upstreamVersion $deployDependencies $preventFork"
    heroku.dynoCreate(app, cmd, "Standard-2X")
  }

  def localDeploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URI] = None, maybeLicense: Option[String] = None): Source[String, Future[NotUsed]] = {

    class ChannelActor extends ActorPublisher[String] {

      val MaxBufferSize = 100
      var buf = Vector.empty[String]

      def receive = {
        case message: String =>
          if (buf.isEmpty && totalDemand > 0)
            onNext(message)
          else {
            buf :+= message
            deliverBuf()
          }
        case Request(_) =>
          deliverBuf()

        case Success(_) =>
          onCompleteThenStop()
        case Failure(e) =>
          onError(e)
      }

      @tailrec final def deliverBuf(): Unit =
        if (totalDemand > 0) {
          if (totalDemand <= Int.MaxValue) {
            val (use, keep) = buf.splitAt(totalDemand.toInt)
            buf = keep
            use foreach onNext
          } else {
            val (use, keep) = buf.splitAt(Int.MaxValue)
            buf = keep
            use foreach onNext
            deliverBuf()
          }
        }
    }

    val actorRef = actorSystem.actorOf(Props(new ChannelActor))
    val actorPublisher = ActorPublisher[String](actorRef)
    val source = Source.fromPublisher(actorPublisher)

    def licenses(packageInfo: PackageInfo, version: String): Future[Map[String, String]] = {
      maybeLicense.fold {
        licenseDetector.resolveLicenses(deployable, packageInfo, Some(version))
      } { license =>
        Future.successful(license.split(",")).map(_.toSet)
      } map LicenseDetector.defaultUrls
    }

    def webJarNotYetDeployed(groupId: String, artifactId: String, version: String): Future[Unit] = {
      mavenCentral.fetchPom(groupId, artifactId, version, Some("https://oss.sonatype.org/content/repositories/releases")).flatMap { elem =>
        Future.failed(new IllegalStateException(s"WebJar $groupId $artifactId $version has already been deployed"))
      } recoverWith {
        case _: FileNotFoundException =>
          Future.successful(Unit)
      }
    }

    def doDeployDependencies(packageInfo: PackageInfo): Future[Done] = {
      if (deployDependencies) {
        actorRef ! "Determining dependency graph"

        deployable.depGraph(packageInfo).flatMap { depGraph =>

          val deployDepGraphMessage = if (depGraph.isEmpty) {
            "No dependencies."
          }
          else {
            "Deploying these dependencies: " + depGraph.map(dep => dep._1 + "#" + dep._2).mkString(" ")
          }

          actorRef ! deployDepGraphMessage

          def deployDep(deps: Map[String, String]): Future[Done] = {
            if (deps.isEmpty) {
              Future.successful(Done)
            }
            else {
              val (nameish, version) = deps.head
              deploy(deployable, nameish, version, false, false).runForeach(actorRef ! _).recover {
                // ignore failures
                case e =>
                  actorRef ! e.getMessage
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

    val future = for {
        packageInfo <- deployable.info(nameOrUrlish, Some(upstreamVersion), maybeSourceUri)
        groupId <- deployable.groupId(nameOrUrlish)
        artifactId <- deployable.artifactId(nameOrUrlish)
        mavenBaseDir = groupId.replaceAllLiterally(".", "/")

        releaseVersion = deployable.releaseVersion(maybeReleaseVersion, packageInfo)

        _ = actorRef ! s"Got package info for $groupId $artifactId $releaseVersion"

        _ <- doDeployDependencies(packageInfo)

        _ <- webJarNotYetDeployed(groupId, artifactId, releaseVersion)

        licenses <- licenses(packageInfo, upstreamVersion)
        _ = actorRef ! s"Resolved Licenses: ${licenses.mkString(",")}"

        mavenDependencies <- deployable.mavenDependencies(packageInfo.dependencies)
        _ = actorRef ! "Converted dependencies to Maven"

        optionalMavenDependencies <- deployable.mavenDependencies(packageInfo.optionalDependencies)
        _ = actorRef ! "Converted optional dependencies to Maven"

        sourceUrl <- sourceLocator.sourceUrl(packageInfo.sourceConnectionUri)
        _ = actorRef ! s"Got the source URL: $sourceUrl"

        pom = templates.xml.pom(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses).toString()
        _ = actorRef ! "Generated POM"

        zip <- deployable.archive(nameOrUrlish, upstreamVersion)
        _ = actorRef ! s"Fetched ${deployable.name} zip"

        excludes <- deployable.excludes(nameOrUrlish, upstreamVersion)

        pathPrefix <- deployable.pathPrefix(nameOrUrlish, releaseVersion, packageInfo)

        jar = WebJarCreator.createWebJar(zip, deployable.contentsInSubdir, excludes, pom, groupId, artifactId, releaseVersion, pathPrefix)

        _ = actorRef ! s"Created ${deployable.name} WebJar"

        packageName = s"$groupId:$artifactId"

        createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $artifactId", Seq("webjar", artifactId), licenses.keySet, packageInfo.sourceConnectionUri, packageInfo.maybeHomepageUrl, packageInfo.maybeIssuesUrl, packageInfo.maybeGitHubOrgRepo)
        _ = actorRef ! "Created BinTray Package"

        createVersion <- binTray.createOrOverwriteVersion(binTraySubject, binTrayRepo, packageName, releaseVersion, s"$artifactId WebJar release $releaseVersion", Some(s"v$releaseVersion"))
        _ = actorRef ! "Created BinTray Version"

        publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion.pom", pom.getBytes)
        publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion.jar", jar)
        emptyJar = WebJarCreator.emptyJar()
        publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion-sources.jar", emptyJar)
        publishJavadocJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/$releaseVersion/$artifactId-$releaseVersion-javadoc.jar", emptyJar)
        _ = actorRef ! "Published BinTray Assets"

        signVersion <- binTray.signVersion(binTraySubject, binTrayRepo, packageName, releaseVersion)
        _ = actorRef ! "Signed BinTray Assets"

        publishVersion <- binTray.publishVersion(binTraySubject, binTrayRepo, packageName, releaseVersion)
        _ = actorRef ! "Published BinTray Version"

        _ = actorRef ! "Syncing to Maven Central (this could take a while)"

        syncToMavenCentral <- binTray.syncToMavenCentral(binTraySubject, binTrayRepo, packageName, releaseVersion)
        _ = actorRef ! "Synced With Maven Central"

        _ = actorRef ! s"""Deployed!
             |It will take a few hours for the Maven Central index to update but you should be able to start using the ${deployable.name} WebJar shortly.
             |GroupID = $groupId
             |ArtifactID = $artifactId
             |Version = $releaseVersion
            """.stripMargin
      } yield NotUsed

    future.onComplete(actorRef ! _)

    source.mapMaterializedValue(_ => future)
  }

  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, preventFork: Boolean, maybeReleaseVersion: Option[String] = None, maybeSourceUri: Option[URI] = None, maybeLicense: Option[String] = None): Source[String, Future[NotUsed]] = {
    if (fork && !preventFork) {
      forkDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, true)
    }
    else {
      localDeploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, maybeReleaseVersion, maybeSourceUri, maybeLicense)
    }
  }

  def create(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, licenseOverride: Option[Map[String, String]], groupIdOverride: Option[String]): Future[(String, Array[Byte])] = {
    import deployable._

    for {
      packageInfo <- deployable.info(nameOrUrlish, Some(upstreamVersion))
      groupId <- groupIdOverride.map(Future.successful).getOrElse(deployable.groupId(nameOrUrlish))
      artifactId <- deployable.artifactId(nameOrUrlish)
      mavenBaseDir = groupId.replaceAllLiterally(".", "/")

      releaseVersion = upstreamVersion.vless

      licenses <- licenseOverride.map(Future.successful).getOrElse(licenseDetector.resolveLicenses(deployable, packageInfo, Some(upstreamVersion)).map(LicenseDetector.defaultUrls))

      mavenDependencies <- deployable.mavenDependencies(packageInfo.dependencies)

      optionalMavenDependencies <- deployable.mavenDependencies(packageInfo.optionalDependencies)

      sourceUrl <- sourceLocator.sourceUrl(packageInfo.sourceConnectionUri)

      pom = templates.xml.pom(groupId, artifactId, releaseVersion, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses).toString()

      zip <- deployable.archive(nameOrUrlish, upstreamVersion)

      excludes <- deployable.excludes(nameOrUrlish, upstreamVersion)

      pathPrefix <- deployable.pathPrefix(nameOrUrlish, releaseVersion, packageInfo)
    } yield artifactId -> WebJarCreator.createWebJar(zip, deployable.contentsInSubdir, excludes, pom, groupId, artifactId, releaseVersion, pathPrefix)
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
    // todo: come up with a way to handle the optional params because if we fork then we lose them
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


    val deployFuture = WebJarType.fromString(webJarType, allDeployables).fold[Future[Done]] {
      Future.failed(new Exception(s"Specified WebJar type '$webJarType' can not be deployed"))
    } { deployable =>
      deployWebJar.deploy(deployable, nameOrUrlish, upstreamVersion, deployDependencies, preventFork, maybeReleaseVersion, maybeSourceUri, maybeLicense).runForeach(println)
    }

    deployFuture.failed.foreach(e => println(e.getMessage))
    deployFuture.onComplete(_ => app.stop())
  }

}

trait Deployable extends WebJarType {

  implicit class RichString(val s: String) {
    def vless: String = s.stripPrefix("v").replaceAllLiterally("^v", "^").replaceAllLiterally("~v", "v")
    def vwith: String = if (s.startsWith("v")) s else "v" + s
  }

  def groupId(nameOrUrlish: String): Future[String]
  def artifactId(nameOrUrlish: String): Future[String]
  def releaseVersion(maybeVersion: Option[String], packageInfo: PackageInfo): String = maybeVersion.getOrElse(packageInfo.version).vless
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
      val maybeVersionRange = SemVer.parseSemVer(version)
      Future.fromTry {
        maybeVersionRange.flatMap { versionRange =>
          SemVer.latestInRange(versionRange, availableVersions).fold[Try[String]] {
            Failure(new Exception("Could not find a valid version in the provided range"))
          } (Success(_))
        }
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
