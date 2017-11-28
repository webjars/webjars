package utils

import java.io.InputStream
import java.net.{URI, URL}
import javax.inject.Inject

import models.WebJarType
import play.api.Logger
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsNull, JsValue}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success, Try}


class DeployWebJar @Inject() (git: Git, binTray: BinTray, pusher: Pusher, maven: Maven, licenseDetector: LicenseDetector, sourceLocator: SourceLocator)(implicit ec: ExecutionContext) {

  def deploy(deployable: Deployable, nameOrUrlish: String, version: String, maybePusherChannelId: Option[String], maybeSourceUri: Option[URI] = None, maybeLicense: Option[String] = None): Future[PackageInfo] = {
    val binTraySubject = "webjars"
    val binTrayRepo = "maven"

    def push(event: String, message: String): Future[JsValue] = {
      maybePusherChannelId.fold {
        Logger.info(message)
        Future.successful[JsValue](JsNull)
      } { pusherChannelId =>
        pusher.push(pusherChannelId, event, message)
      }
    }

    def licenses(packageInfo: PackageInfo, version: String): Future[Set[String]] = {
      maybeLicense.fold {
        licenseDetector.resolveLicenses(deployable, packageInfo, Some(version))
      } { license =>
        Future.successful(Set(license))
      }
    }

    val deployFuture = for {
      packageInfo <- deployable.info(nameOrUrlish, Some(version), maybeSourceUri)
      groupId <- deployable.groupId(packageInfo).fold(Future.failed[String](new Exception("Could not groupId")))(Future.successful)
      artifactId <- deployable.artifactId(packageInfo).fold(Future.failed[String](new Exception("Could not determine artifactId")))(Future.successful)
      mavenBaseDir <- deployable.mavenBaseDir(packageInfo).fold(Future.failed[String](new Exception("Could not determine mavenBaseDir")))(Future.successful)
      _ <- push("update", s"Deploying $groupId $artifactId $version")
      licenses <- licenses(packageInfo, version)
      _ <- push("update", "Resolved Licenses")
      mavenDependencies <- maven.convertNpmBowerDependenciesToMaven(packageInfo.dependencies)
      _ <- push("update", "Converted dependencies to Maven")
      optionalMavenDependencies <- maven.convertNpmBowerDependenciesToMaven(packageInfo.optionalDependencies)
      _ <- push("update", "Converted optional dependencies to Maven")
      sourceUrl <- sourceLocator.sourceUrl(packageInfo.sourceConnectionUri)
      _ <- push("update", s"Got the source URL: $sourceUrl")
      pom = templates.xml.pom(groupId, artifactId, packageInfo, sourceUrl, mavenDependencies, optionalMavenDependencies, licenses).toString()
      _ <- push("update", "Generated POM")
      zip <- deployable.archive(nameOrUrlish, version)
      _ <- push("update", s"Fetched ${deployable.name} zip")
      jar = WebJarCreator.createWebJar(zip, deployable.contentsInSubdir, deployable.excludes, pom, groupId, artifactId, packageInfo.version)
      _ <- push("update", s"Created ${deployable.name} WebJar")

      packageName = s"$groupId:$artifactId"

      createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $artifactId", Seq("webjar", artifactId), licenses, packageInfo.sourceConnectionUri, packageInfo.maybeHomepageUrl, packageInfo.maybeIssuesUrl, packageInfo.maybeGitHubOrgRepo)
      _ <- push("update", "Created BinTray Package")

      createVersion <- binTray.createOrOverwriteVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version, s"$artifactId WebJar release ${packageInfo.version}", Some(s"v${packageInfo.version}"))
      _ <- push("update", "Created BinTray Version")
      publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}.pom", pom.getBytes)
      publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}.jar", jar)
      emptyJar = WebJarCreator.emptyJar()
      publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}-sources.jar", emptyJar)
      publishJavadocJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}-javadoc.jar", emptyJar)
      _ <- push("update", "Published BinTray Assets")
      signVersion <- binTray.signVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version)
      _ <- push("update", "Signed BinTray Assets")
      publishVersion <- binTray.publishVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version)
      _ <- push("update", "Published BinTray Version")

      syncToMavenCentral <- binTray.syncToMavenCentral(binTraySubject, binTrayRepo, packageName, packageInfo.version)
      _ <- push("update", "Synced With Maven Central")
      _ <- push("success",
        s"""Deployed!
           |It will take a few hours for the Maven Central index to update but you should be able to start using the ${deployable.name} WebJar now.
           |GroupID = $groupId
           |ArtifactID = $artifactId
           |Version = ${packageInfo.version}
        """.stripMargin)
    } yield packageInfo

    deployFuture.recoverWith {
      // push the error out and then return the original error
      case e: Exception =>
        push(
          "failure",
          s"""Failed!
             |${e.getMessage}
             |If you feel you have reached this failure in error, please file an issue: https://github.com/webjars/webjars/issues
          """.stripMargin).flatMap(_ => Future.failed(e))
    }
  }

}

object DeployWebJar extends App {

  val (webJarType, nameOrUrlish, version, maybePusherChannelId, maybeSourceUri, maybeLicense) = if (args.length < 3) {
    val webJarType = StdIn.readLine("WebJar Type: ")
    val nameOrUrlish = StdIn.readLine("Name or URL: ")
    val version = StdIn.readLine("Version: ")
    val sourceUriIn = StdIn.readLine("Source URI (override): ")
    val licenseIn = StdIn.readLine("License (override): ")

    val maybeSourceUri = if (sourceUriIn.isEmpty) {
      None
    }
    else {
      Try(new URI(sourceUriIn)).toOption
    }

    val maybeLicense = if (licenseIn.isEmpty) {
      None
    }
    else {
      Some(licenseIn)
    }

    (webJarType, nameOrUrlish, version, None, maybeSourceUri, maybeLicense)
  }
  else {
    val maybePusherChannelId = if (args.length == 4) {
      Some(args(3))
    } else {
      None
    }

    (args(0), args(1), args(2), maybePusherChannelId, None, None)
  }

  if (nameOrUrlish.isEmpty || version.isEmpty) {
    println("Name and version must be specified")
    sys.exit(1)
  }
  else {
    val app = new GuiceApplicationBuilder().build()

    val deployWebJar = app.injector.instanceOf[DeployWebJar]

    val npm = app.injector.instanceOf[NPM]
    val bower = app.injector.instanceOf[Bower]

    val allDeployables = Set(npm, bower)

    WebJarType.fromString(webJarType, allDeployables).fold {
      println(s"Specified WebJar type '$webJarType' can not be deployed")
      sys.exit(1)
    } { deployable =>
      deployWebJar.deploy(deployable, nameOrUrlish, version, maybePusherChannelId, maybeSourceUri, maybeLicense).onComplete {
        case Success(s) =>
          println("Done!")
          app.stop()
        case Failure(f) =>
          println("Error: ", f)
          app.stop()
      } (ExecutionContext.global)
    }
  }

}

trait Deployable extends WebJarType {
  def groupId(packageInfo: PackageInfo): Option[String]
  def artifactId(packageInfo: PackageInfo): Option[String]
  val excludes: Set[String]
  val metadataFile: String
  val contentsInSubdir: Boolean
  def mavenBaseDir(packageInfo: PackageInfo): Option[String] = groupId(packageInfo).map(_.replaceAllLiterally(".", "/"))
  def info(nameOrUrlish: String, maybeVersion: Option[String], maybeSourceUri: Option[URI]): Future[PackageInfo]
  def archive(nameOrUrlish: String, version: String): Future[InputStream]
}
