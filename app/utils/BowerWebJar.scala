package utils

import java.io.File

import play.api.libs.json.{JsValue, Json}
import play.api.{Configuration, Mode}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object BowerWebJar extends App {

  def release(name: String, version: String, maybepusherChannelId: Option[String])(implicit executionContext: ExecutionContext, config: Configuration): Future[PackageInfo] = {

    val binTraySubject = "webjars"
    val binTrayRepo = "maven"
    val groupId = "org.webjars.bower"
    val mavenBaseDir = "org/webjars/bower"

    // converts JsResult to Future
    def packageInfo(json: JsValue): Future[PackageInfo] = Json.fromJson[PackageInfo](json).fold(
      errors => Future.failed(new Exception(errors.toString())),
      Future.successful
    )

    def convertNpmDependenciesToMaven(npmDependencies: Map[String, String]): Future[Map[String, String]] = {
      val maybeMavenDeps = npmDependencies.map { case (npmName, npmVersion) =>
        val maybeMavenVersion = SemVerUtil.convertSemVerToMaven(npmVersion)
        maybeMavenVersion.fold {
          Future.failed[(String, String)](new Exception(s"Could not convert npm version to maven for: $npmName $npmVersion"))
        } { mavenVersion =>
          Future.successful(npmName -> mavenVersion)
        }
      }

      Future.sequence(maybeMavenDeps).map(_.toMap)
    }

    StandaloneWS.withWs { implicit ws =>

      val bower = Bower(executionContext, ws.client)
      val binTray = BinTray(executionContext, ws, config)
      val pusher = Pusher(executionContext, ws.client, config)

      def push(event: String, message: String): Unit = {
        maybepusherChannelId.foreach { pusherChannelId =>
          pusher.push(pusherChannelId, event, message)
        }
      }
      
      val webJarFuture = for {
        packageInfo <- bower.info(name, version)
        _ = push("update", "Got Bower info: " + packageInfo.toString)
        mavenDependencies <- convertNpmDependenciesToMaven(packageInfo.dependencies)
        _ = push("update", "Converted dependencies to Maven: " + mavenDependencies.toString())
        pom = views.xml.pom(packageInfo, mavenDependencies).toString()
        _ = push("update", "Generated POM: " + pom.toString)
        zip <- bower.zip(packageInfo.artifactId, version)
        _ = push("update", "Fetched Bower zip")
        jar = WebJarUtils.createWebJar(zip, pom, packageInfo.artifactId, version)
        _ = push("update", "Created WebJar")
      } yield (packageInfo, pom, jar)
      
      val binTrayFuture = webJarFuture.flatMap { case (packageInfo, pom, jar) =>
        // do not used the provided name because it may not be the canonical name
        val name = packageInfo.artifactId
        val packageName = s"$groupId:$name"

        for {
          createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $name", Seq("webjar", name), packageInfo.licenses, packageInfo.source, Some(packageInfo.homepage), packageInfo.issuesUrl.toOption, packageInfo.gitHubOrgRepo.toOption)
          _ = push("update", "Created BinTray Package:" + createPackage.toString)
          createVersion <- binTray.createVersion(binTraySubject, binTrayRepo, packageName, version, s"$name WebJar release $version", Some(s"v$version"))
          _ = push("update", "Created BinTray Version:" + createVersion.toString)
          publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$name/$version/$name-$version.pom", pom.getBytes)
          publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$name/$version/$name-$version.jar", jar)
          emptyJar = WebJarUtils.emptyJar()
          publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$name/$version/$name-$version-sources.jar", emptyJar)
          _ = push("update", "Published BinTray Assets")
          signVersion <- binTray.signVersion(binTraySubject, binTrayRepo, packageName, version)
          _ = push("update", "Signed BinTray Assets")
          publishVersion <- binTray.publishVersion(binTraySubject, binTrayRepo, packageName, version)
          _ = push("update", "Published BinTray Version")
          syncToMavenCentral <- binTray.syncToMavenCentral(binTraySubject, binTrayRepo, packageName, version)
          _ = push("update", "Synced With Maven Central")
        } yield syncToMavenCentral
      }

      binTrayFuture.onComplete {
        case Success(s) =>
          push("success", "Deployed! " + s.toString)
        case Failure(f) =>
          push("failure", "Failed! " + f.getMessage)
      }

      // return the packageInfo
      binTrayFuture.flatMap(_ => webJarFuture.map(_._1))
    }
  }

  // Don't let the Play Akka Scheduler block exiting
  System.setProperty("play.akka.daemonic", "on")

  if (args.length < 2) {
    println("You must specify the Bower artifact name and version")
  }
  else {
    val name = args(0)
    val version = args(1)
    val maybepusherChannelId = if (args.length == 3) { Some(args(2)) } else { None }

    val config = Configuration.load(new File("."), Mode.Prod)

    release(name, version, maybepusherChannelId)(ExecutionContext.global, config).onComplete {
      case Success(s) =>
        println("Done!")
      case Failure(f) =>
        println("Error: ", f)
    }(ExecutionContext.global)
  }

}
