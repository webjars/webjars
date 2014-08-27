package controllers

import play.api.mvc.{Result, Request, Action, Controller}
import utils.MavenCentral

import scala.concurrent.ExecutionContext.Implicits.global
import java.util.jar.JarFile
import java.io.IOException
import play.api.libs.iteratee.Enumerator
import play.api.libs.MimeTypes
import play.api.Play
import play.api.Play.current

import scala.collection.JavaConverters._
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.DateTimeZone

import scala.concurrent.Future

object Application extends Controller {

  def index = Action.async {
    MavenCentral.allWebJars.map { allWebJars =>
      Ok(views.html.index(allWebJars))
    }
  }
  
  def listFiles(artifactId: String, version: String) = Action.async {
    MavenCentral.listFiles(artifactId, version).map { fileList =>
      Ok(views.html.filelist(artifactId, version, fileList))
    }
  }
  
  def file(artifactId: String, webJarVersion: String, file: String) = CorsAction {
    Action { request =>
      val maybeJarFile: Option[JarFile] = MavenCentral.getFile(artifactId, webJarVersion)

      maybeJarFile match {
        case Some(jarFile) =>
          val pathPrefix = s"META-INF/resources/webjars/$artifactId/"
          val maybeEntry = jarFile.entries().asScala.filter { entry =>
            entry.getName.startsWith(pathPrefix) && entry.getName.endsWith(s"/$file")
          }.toList.headOption

          maybeEntry match {
            case None =>
              jarFile.close()
              NotFound(s"Found WebJar but could not find a file matching: $pathPrefix$webJarVersion/$file")
            case Some(entry) =>
              try {
                val inputStream = jarFile.getInputStream(entry)
                val enumerator: Enumerator[Array[Byte]] = Enumerator.fromStream(inputStream)
                enumerator.onDoneEnumerating(jarFile.close())

                //// From Play's Assets controller
                val contentType = MimeTypes.forFileName(file).map(m => m + addCharsetIfNeeded(m)).getOrElse(BINARY)
                ////

                Ok.feed(enumerator).as(contentType).withHeaders(
                  CACHE_CONTROL -> "max-age=290304000, public",
                  DATE -> df.print({ new java.util.Date }.getTime),
                  LAST_MODIFIED -> df.print(entry.getLastModifiedTime.toMillis)
                )
              }
              catch {
                case e: IOException =>
                  jarFile.close()
                  NotFound(s"Found WebJar but could not read file: ${entry.getName}\nError: ${e.getMessage}")
              }
          }
        case None =>
          NotFound(s"Could not find WebJar: $artifactId $webJarVersion")
      }
    }
  }

  def fileOptions(artifactId: String, version: String, file: String) = CorsAction {
    Action { request =>
      Ok.withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> Seq(CONTENT_TYPE).mkString(","))
    }
  }

  def documentation = Action {
    Ok(views.html.documentation())
  }

  def contributing = Action {
    Ok(views.html.contributing())
  }

  case class CorsAction[A](action: Action[A]) extends Action[A] {

    def apply(request: Request[A]): Future[Result] = {
      action(request).map(result => result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> "*"))
    }

    lazy val parser = action.parser
  }

  //// From Play's Asset controller

  private val timeZoneCode = "GMT"

  //Dateformatter is immutable and threadsafe
  private val df: DateTimeFormatter =
    DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss '" + timeZoneCode + "'").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))

  //Dateformatter is immutable and threadsafe
  private val dfp: DateTimeFormatter =
    DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))

  private lazy val defaultCharSet = Play.configuration.getString("default.charset").getOrElse("utf-8")

  private def addCharsetIfNeeded(mimeType: String): String =
    if (MimeTypes.isText(mimeType))
      "; charset=" + defaultCharSet
    else ""

  ////

}