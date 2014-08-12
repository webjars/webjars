package controllers

import play.api.mvc.{Action, Controller}
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

object Application extends Controller {

  val primaryBaseJarUrl = Play.current.configuration.getString("webjars.jarUrl.primary").get
  val fallbackBaseJarUrl = Play.current.configuration.getString("webjars.jarUrl.fallback").get

  def index = Action.async {
    MavenCentral.allWebJars.map { allWebJars =>
      Ok(views.html.index(allWebJars))
    }
  }
  
  def listFiles(artifactId: String, version: String) = Action {
    Ok(MavenCentral.listFiles(primaryBaseJarUrl, artifactId, version))
  }
  
  def file(artifactId: String, webJarVersion: String, file: String) = Action { request =>

    val maybeJarFile: Option[JarFile] = MavenCentral.getFile(primaryBaseJarUrl, artifactId, webJarVersion).orElse {
      MavenCentral.getFile(fallbackBaseJarUrl, artifactId, webJarVersion)
    }

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

  def documentation = Action {
    Ok(views.html.documentation())
  }

  def contributing = Action {
    Ok(views.html.contributing())
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