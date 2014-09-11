package controllers

import models.WebJar
import play.api.libs.json.Json
import play.api.mvc.{Result, Request, Action, Controller}
import utils.MavenCentral
import utils.MavenCentral.{NotFoundResponseException, UnexpectedResponseException}

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.iteratee.Enumerator
import play.api.libs.MimeTypes
import play.api.Play
import play.api.Play.current

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.DateTimeZone

import scala.concurrent.Future
import scala.util.hashing.MurmurHash3

object Application extends Controller {

  def index = Action.async { implicit request =>
    MavenCentral.allWebJars.map { allWebJars =>
      val acceptHash = MurmurHash3.seqHash(request.acceptedTypes)
      val hash = MurmurHash3.listHash(allWebJars, acceptHash)
      val etag = "\"" + hash + "\""
      if (request.headers.get(IF_NONE_MATCH).contains(etag)) {
        NotModified
      }
      else {
        Ok(views.html.index(allWebJars)).withHeaders(ETAG -> etag)
      }
    } recover {
      case e: Exception =>
        InternalServerError(views.html.index(Seq.empty[WebJar]))
    }
  }

  def allWebJars = Action.async { implicit request =>
    MavenCentral.allWebJars.map { allWebJars =>
      val acceptHash = MurmurHash3.seqHash(request.acceptedTypes)
      val hash = MurmurHash3.listHash(allWebJars, acceptHash)
      val etag = "\"" + hash + "\""
      if (request.headers.get(IF_NONE_MATCH).contains(etag)) {
        NotModified
      }
      else {
        Ok(Json.toJson(allWebJars)).withHeaders(ETAG -> etag)
      }
    } recover {
      case e: Exception =>
        InternalServerError(Json.arr())
    }
  }
  
  def listFiles(artifactId: String, version: String) = Action.async {
    MavenCentral.getFileList(artifactId, version).map { fileList =>
      Ok(views.html.filelist(artifactId, version, fileList))
    } recover {
      case nf: NotFoundResponseException =>
        NotFound(s"WebJar Not Found $artifactId : $version")
      case ure: UnexpectedResponseException =>
        Status(ure.response.status)(s"Problems retrieving WebJar ($artifactId : $version) - ${ure.response.statusText}")
    }
  }
  
  def file(artifactId: String, webJarVersion: String, file: String) = CorsAction {
    Action.async { request =>
      val pathPrefix = s"META-INF/resources/webjars/$artifactId/"

      MavenCentral.getFile(artifactId, webJarVersion).map { jarInputStream =>
        Stream.continually(jarInputStream.getNextJarEntry).takeWhile(_ != null).find { jarEntry =>
          // this allows for sloppyness where the webJarVersion and path differ
          // todo: eventually be more strict but since this has been allowed many WebJars do not have version and path consistency
          jarEntry.getName.startsWith(pathPrefix) && jarEntry.getName.endsWith(s"/$file")
        }.fold {
          jarInputStream.close()
          NotFound(s"Found WebJar ($artifactId : $webJarVersion) but could not find: $pathPrefix$webJarVersion/$file")
        } { jarEntry =>
          val enumerator = Enumerator.fromStream(jarInputStream)
          enumerator.onDoneEnumerating(jarInputStream.close())

          //// From Play's Assets controller
          val contentType = MimeTypes.forFileName(file).map(m => m + addCharsetIfNeeded(m)).getOrElse(BINARY)
          ////

          Ok.feed(enumerator).as(contentType).withHeaders(
            CACHE_CONTROL -> "max-age=290304000, public",
            DATE -> df.print({ new java.util.Date }.getTime),
            LAST_MODIFIED -> df.print(jarEntry.getLastModifiedTime.toMillis)
          )
        }
      } recover {
        case nf: NotFoundResponseException =>
          NotFound(s"WebJar Not Found $artifactId : $webJarVersion")
        case ure: UnexpectedResponseException =>
          Status(ure.response.status)(s"Problems retrieving WebJar ($artifactId : $webJarVersion) - ${ure.response.statusText}")
        case e: Exception =>
          InternalServerError(s"Could not find WebJar ($artifactId : $webJarVersion)\n${e.getMessage}")
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