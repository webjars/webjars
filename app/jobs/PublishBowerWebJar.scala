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
import utils.{WebJarUtils, StandaloneWS, PackageInfo, Bower}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object PublishBowerWebJar extends App {

  // Don't let the Play Akka Scheduler block exiting
  System.setProperty("play.akka.daemonic", "on")

  val name = "bootstrap"
  val version = "1.0.0"

  // converts JsResult to Future
  def packageInfo(json: JsValue): Future[PackageInfo] = Json.fromJson[PackageInfo](json).fold(
    errors => Future.failed(new Exception(errors.toString())),
    Future.successful
  )

  StandaloneWS.withWs { implicit ws =>
    val bower = Bower(global, ws)

    val webJarFuture = for {
      packageInfo <- bower.info(name, version)
      pom = views.xml.pom(packageInfo).toString()
      zip <- bower.zip(name, version)
      jar = WebJarUtils.createWebJar(zip, pom, name, version)
    } yield {
      (pom, jar)
    }

    webJarFuture

  } onComplete {
    case Success(s) =>
      println("Done!")
    case Failure(f) =>
      println("Error: ", f)
  }

  // create package & version on BinTray

  // upload pom.xml and foo.jar to BinTray

  // sign on BinTray

  // release to Maven Central

}