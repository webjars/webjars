package utils

import java.io.InputStream
import javax.inject.Inject

import play.api.Logger
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsNull, JsValue}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


class DeployWebJar @Inject() (git: Git, binTray: BinTray, pusher: Pusher, maven: Maven, licenseDetector: LicenseDetector) (implicit ec: ExecutionContext) {

  def deploy[A](nameOrUrlish: String, version: String, maybePusherChannelId: Option[String])(implicit deployable: Deployable[A]): Future[PackageInfo[A]] = {
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

    val deployFuture = for {
      _ <- push("update", s"Deploying ${deployable.groupId} $nameOrUrlish $version")
      artifactId <- git.artifactId(nameOrUrlish)
      _ <- push("update", s"Determined Artifact Name: $artifactId")
      packageInfo <- deployable.info(nameOrUrlish, Some(version))
      _ <- push("update", s"Got ${deployable.name} info")
      licenses <- licenseDetector.resolveLicenses(packageInfo, Some(version))
      _ <- push("update", "Resolved Licenses")
      mavenDependencies <- maven.convertNpmBowerDependenciesToMaven(packageInfo.dependencies)
      _ <- push("update", "Converted dependencies to Maven")
      pom = templates.xml.pom(deployable.groupId, artifactId, packageInfo, mavenDependencies, licenses).toString()
      _ <- push("update", "Generated POM")
      zip <- deployable.archive(nameOrUrlish, version)
      _ <- push("update", s"Fetched ${deployable.name} zip")
      jar = WebJarCreator.createWebJar(zip, deployable.contentsInSubdir, deployable.excludes, pom, deployable.groupId, artifactId, packageInfo.version)
      _ <- push("update", s"Created ${deployable.name} WebJar")

      packageName = s"${deployable.groupId}:$artifactId"

      createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $artifactId", Seq("webjar", artifactId), licenses, packageInfo.sourceUrl, Some(packageInfo.homepageUrl), Some(packageInfo.issuesUrl), packageInfo.gitHubOrgRepo)
      _ <- push("update", "Created BinTray Package")

      // do not fail if the binTray version already exists
      createVersion <- binTray.createVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version, s"$artifactId WebJar release ${packageInfo.version}", Some(s"v${packageInfo.version}")).recover { case e: BinTray.VersionExists => e.getMessage }
      _ <- push("update", "Created BinTray Version")
      publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"${deployable.mavenBaseDir}/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}.pom", pom.getBytes)
      publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"${deployable.mavenBaseDir}/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}.jar", jar)
      emptyJar = WebJarCreator.emptyJar()
      publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"${deployable.mavenBaseDir}/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}-sources.jar", emptyJar)
      publishJavadocJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"${deployable.mavenBaseDir}/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}-javadoc.jar", emptyJar)
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
           |GroupID = ${deployable.groupId}
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

  if (args.length < 2) {
    println("You must specify the NPM artifact name and version")
    sys.exit(1)
  }
  else {
    val groupId = args(0)
    val nameOrUrlish = args(1)
    val version = args(2)
    val maybePusherChannelId = if (args.length == 4) { Some(args(3)) } else { None }

    val app = new GuiceApplicationBuilder().build()

    val deployWebJar = app.injector.instanceOf[DeployWebJar]

    implicit val deployable = groupId match {
      case NPM.groupId => NPM.deployable(app.injector.instanceOf[NPM])
      case Bower.groupId => Bower.deployable(app.injector.instanceOf[Bower])
    }

    deployWebJar.deploy(nameOrUrlish, version, maybePusherChannelId).onComplete {
      case Success(s) =>
        println("Done!")
        app.stop()
      case Failure(f) =>
        println("Error: ", f)
        app.stop()
    } (ExecutionContext.global)
  }

}

trait Deployable[A] {
  val name: String
  val groupId: String
  val excludes: Set[String]
  val metadataFile: String
  val contentsInSubdir: Boolean
  def mavenBaseDir: String = groupId.replaceAllLiterally(".", "/")
  def info(nameOrUrlish: String, maybeVersion: Option[String]): Future[PackageInfo[A]]
  def archive(nameOrUrlish: String, version: String): Future[InputStream]
}
