package jobs

import java.io._
import java.nio.file.Files
import java.util.jar.JarOutputStream
import java.util.zip.{ZipInputStream, ZipEntry}

import play.api.libs.concurrent.Akka
import play.api.{Play, Mode, DefaultApplication}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import utils._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{Failure, Success}

object PublishBowerWebJar extends App {

  // Don't let the Play Akka Scheduler block exiting
  System.setProperty("play.akka.daemonic", "on")

  val name = "bootstrap"
  val version = "3.3.2"

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
    val bower = Bower(global, ws)

    val webJarFuture = for {
      packageInfo <- bower.info(name, version)
      mavenDependencies <- convertNpmDependenciesToMaven(packageInfo.dependencies)
      pom = views.xml.pom(packageInfo, mavenDependencies).toString()
      zip <- bower.zip(name, version)
      jar = WebJarUtils.createWebJar(zip, pom, name, version)
    } yield (pom, jar)

    webJarFuture

    // create package & version on BinTray

    // upload pom.xml and foo.jar to BinTray

    // sign on BinTray

    // release to Maven Central

  } onComplete {
    case Success(s) =>
      println("Done!")
    case Failure(f) =>
      println("Error: ", f)
  }



}