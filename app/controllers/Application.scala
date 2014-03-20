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

object Application extends Controller {
    
  def index = Action.async {
    MavenCentral.allWebJars.map { allWebJars =>
      Ok(views.html.index(allWebJars))
    }
  }
  
  def listFiles(artifactId: String, version: String) = Action {
    Ok(MavenCentral.listFiles(artifactId, version))
  }
  
  def file(artifactId: String, version: String, file: String) = Action { request =>
    
    val maybeJarFile: Option[JarFile] = MavenCentral.getFile(artifactId, version)
  
    maybeJarFile match {
      case Some(jarFile) =>
        val filePath = s"META-INF/resources/webjars/$artifactId/$version/$file"
        val entry = jarFile.getEntry(filePath)
        if (entry == null) {
          NotFound("Found WebJar but could not find file: " + filePath)
        }
        else {
          try {
            // todo: etag / 304 support
            val inputStream = jarFile.getInputStream(entry)
            val enumerator: Enumerator[Array[Byte]] = Enumerator.fromStream(inputStream)
            
            // From Play's Assets controller
            val contentType = MimeTypes.forFileName(file).map(m => m + addCharsetIfNeeded(m)).getOrElse(BINARY)
            //
            
            Ok.feed(enumerator).withHeaders("Cache-Control" -> "max-age=290304000, public").as(contentType)
          }
          catch {
            case e: IOException =>
              NotFound("Found WebJar but could not read file: " + filePath + "\nError: " + e.getMessage)
          }
        }
      case None =>
        NotFound(s"Could not find WebJar: $artifactId $version")
    }
  }

  def documentation = Action {
    Ok(views.html.documentation())
  }

  def contributing = Action {
    Ok(views.html.contributing())
  }

  // From Play's Assets controller
  private lazy val defaultCharSet = Play.configuration.getString("default.charset").getOrElse("utf-8")

  private def addCharsetIfNeeded(mimeType: String): String =
    if (MimeTypes.isText(mimeType))
      "; charset=" + defaultCharSet
    else ""
  //
  
}