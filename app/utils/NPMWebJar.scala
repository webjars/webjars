package utils

import java.io.File

import play.api.libs.json.{JsNull, JsValue}
import play.api.{Configuration, Mode}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object NPMWebJar extends App {

  def release(nameOrUrlish: String, version: String, maybepusherChannelId: Option[String])(implicit executionContext: ExecutionContext, config: Configuration): Future[PackageInfo] = {

    val binTraySubject = "webjars"
    val binTrayRepo = "maven"
    val groupId = "org.webjars.npm"
    val mavenBaseDir = "org/webjars/npm"

    StandaloneWS.withWs { implicit ws =>

      val npm = NPM(executionContext, ws.client)
      val git = GitUtil(executionContext, ws.client)
      val binTray = BinTray(executionContext, ws, config)
      val pusher = Pusher(executionContext, ws.client, config)
      val maven = MavenUtil(executionContext, git)

      def push(event: String, message: String): Future[JsValue] = {
        maybepusherChannelId.fold(Future.successful[JsValue](JsNull)) { pusherChannelId =>
          pusher.push(pusherChannelId, event, message)
        }
      }

      val webJarFuture = for {
        artifactId <- git.artifactId(nameOrUrlish)
        _ <- push("update", s"Determined Artifact Name: $artifactId")
        packageInfo <- npm.info(nameOrUrlish, Some(version))
        _ <- push("update", "Got NPM info")
        mavenDependencies <- maven.convertNpmBowerDependenciesToMaven(packageInfo.dependencies)
        _ <- push("update", "Converted dependencies to Maven")
        pom = templates.xml.pom(groupId, artifactId, packageInfo, mavenDependencies).toString()
        _ <- push("update", "Generated POM")
        tgz <- npm.tgz(nameOrUrlish, version)
        _ <- push("update", "Fetched NPM tgz")
        jar = WebJarUtils.createWebJar(tgz, "package/", Set("node_modules"), pom, groupId, artifactId, packageInfo.version)
        _ <- push("update", "Created NPM WebJar")
      } yield (artifactId, packageInfo, pom, jar)

      webJarFuture.flatMap { case (artifactId, packageInfo, pom, jar) =>
        val packageName = s"$groupId:$artifactId"

        for {
          licensesForBinTray <- binTray.convertLicenses(packageInfo.licenses, packageInfo.sourceConnectionUrl)
          _ <- push("update", "Converted project licenses")
          createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $artifactId", Seq("webjar", artifactId), licensesForBinTray, packageInfo.sourceUrl, Some(packageInfo.homepage), Some(packageInfo.issuesUrl), packageInfo.gitHubOrgRepo.toOption)
          _ <- push("update", "Created BinTray Package")

          binTrayPublishFuture = for {
            createVersion <- binTray.createVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version, s"$artifactId WebJar release ${packageInfo.version}", Some(s"v${packageInfo.version}"))
            _ <- push("update", "Created BinTray Version")
            publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}.pom", pom.getBytes)
            publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}.jar", jar)
            emptyJar = WebJarUtils.emptyJar()
            publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}-sources.jar", emptyJar)
            publishJavadocJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$artifactId/${packageInfo.version}/$artifactId-${packageInfo.version}-javadoc.jar", emptyJar)
            _ <- push("update", "Published BinTray Assets")
            signVersion <- binTray.signVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version)
            _ <- push("update", "Signed BinTray Assets")
            publishVersion <- binTray.publishVersion(binTraySubject, binTrayRepo, packageName, packageInfo.version)
            _ <- push("update", "Published BinTray Version")
          } yield (createVersion, publishPom, publishPom, publishSourceJar, publishJavadocJar, signVersion, publishVersion)

          // do not fail if the binTray version already exists
          binTrayPublish <- binTrayPublishFuture.recover { case e: BinTray.VersionExists => e.getMessage }

          syncToMavenCentral <- binTray.syncToMavenCentral(binTraySubject, binTrayRepo, packageName, packageInfo.version)
          _ <- push("update", "Synced With Maven Central")
          _ <- push("success",
            s"""Deployed!
              |It will take a few hours for the Maven Central index to update but you should be able to start using the NPM WebJar now.
              |GroupID = $groupId
              |ArtifactID = $artifactId
              |Version = ${packageInfo.version}
            """.stripMargin)
        } yield packageInfo
      } recoverWith {
        // push the error out and then return the original error
        case e: Exception =>
          push("failure",
            s"""Failed!
              |${e.getMessage}
              |If you feel you have reached this failure in error, please file an issue: https://github.com/webjars/webjars/issues
            """.stripMargin).flatMap(_ => Future.failed(e))
      }
    }
  }

  // Don't let the Play Akka Scheduler block exiting
  System.setProperty("play.akka.daemonic", "on")

  if (args.length < 2) {
    println("You must specify the NPM artifact name and version")
    sys.exit(1)
  }
  else {
    val name = args(0)
    val version = args(1)
    val maybepusherChannelId = if (args.length == 3) { Some(args(2)) } else { None }

    val config = Configuration.load(new File("."), Mode.Prod)

    release(name, version, maybepusherChannelId)(ExecutionContext.global, config).onComplete {
      case Success(s) =>
        println("Done!")
        sys.exit()
      case Failure(f) =>
        println("Error: ", f)
        sys.exit()
    } (ExecutionContext.global)
  }

}
