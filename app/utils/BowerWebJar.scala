package utils

import java.io.File

import play.api.libs.json.{JsValue, Json}
import play.api.{Configuration, Mode}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object BowerWebJar extends App {

  val binTraySubject = "webjars"
  val binTrayRepo = "maven"
  val groupId = "org.webjars.bower"
  val mavenBaseDir = "org/webjars/bower"


  def release(name: String, version: String)(implicit executionContext: ExecutionContext, config: Configuration): Future[PackageInfo] = {

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

      val webJarFuture = for {
        packageInfo <- bower.info(name, version)
        mavenDependencies <- convertNpmDependenciesToMaven(packageInfo.dependencies)
        pom = views.xml.pom(packageInfo, mavenDependencies).toString()
        zip <- bower.zip(name, version)
        jar = WebJarUtils.createWebJar(zip, pom, name, version)
      } yield (packageInfo, pom, jar)
      
      val binTrayFuture = webJarFuture.flatMap { case (packageInfo, pom, jar) =>
        println("Created Bower WebJar: " + packageInfo)

        val packageName = s"$groupId:$name"
        for {
          createPackage <- binTray.getOrCreatePackage(binTraySubject, binTrayRepo, packageName, s"WebJar for $name", Seq("webjar", name), packageInfo.licenses, packageInfo.source, Some(packageInfo.homepage), packageInfo.issuesUrl.toOption, packageInfo.gitHubOrgRepo.toOption)
          createVersion <- binTray.createVersion(binTraySubject, binTrayRepo, packageName, version, s"$name WebJar release $version", Some(s"v$version"))
          publishPom <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$name/$version/$name-$version.pom", pom.getBytes)
          publishJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$name/$version/$name-$version.jar", jar)
          emptyJar = WebJarUtils.emptyJar()
          publishSourceJar <- binTray.uploadMavenArtifact(binTraySubject, binTrayRepo, packageName, s"$mavenBaseDir/$name/$version/$name-$version-sources.jar", emptyJar)
          signVersion <- binTray.signVersion(binTraySubject, binTrayRepo, packageName, version)
          publishVersion <- binTray.publishVersion(binTraySubject, binTrayRepo, packageName, version)
          syncToMavenCentral <- binTray.syncToMavenCentral(binTraySubject, binTrayRepo, packageName, version)
        } yield syncToMavenCentral
      }

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

    val config = Configuration.load(new File("."), Mode.Prod)

    release(name, version)(ExecutionContext.global, config).onComplete {
      case Success(s) =>
        println("Done!")
      case Failure(f) =>
        println("Error: ", f)
    }(ExecutionContext.global)
  }

}
