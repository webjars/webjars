package utils

import java.io.{File, InputStream}
import java.net.URL
import java.nio.file.Files
import java.util.zip.GZIPInputStream

import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.util.FileUtils
import play.api.http.{HeaderNames, Status}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc.Results

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Try
import scala.collection.JavaConverters._

class Git(implicit ec: ExecutionContext, ws: WSClient) {

  val cacheDir = Files.createTempDirectory("git")

  val licenseUtils = LicenseUtils(ec, ws)

  def gitUrl(gitRepo: String): Future[String] = {
    val resolvedUrl = if (gitRepo.contains("://")) {
      gitRepo
    }
    else {
      // infer github
      s"https://github.com/$gitRepo"
    }

    val jgitReadyUrl = resolvedUrl.replaceAllLiterally("git+", "")

    if (jgitReadyUrl.startsWith("http")) {
      ws.url(jgitReadyUrl).withFollowRedirects(false).head().flatMap { response =>
        response.status match {
          case Status.MOVED_PERMANENTLY =>
            response.header(HeaderNames.LOCATION).fold(Future.failed[String](new Exception("Could not get redir location")))(Future.successful)
          case Status.OK =>
            Future.successful(jgitReadyUrl)
          case _ =>
            Future.failed(new Exception(s"Could not get HEAD for url: $jgitReadyUrl"))
        }
      }
    }
    else {
      Future.successful(jgitReadyUrl)
    }
  }

  def versions(gitRepo: String): Future[Seq[String]] = {
    gitUrl(gitRepo).flatMap { url =>
      Future.fromTry {
        Try {
          val tags = org.eclipse.jgit.api.Git.lsRemoteRepository()
            .setRemote(url)
            .setTags(true)
            .call()

          tags.asScala.map(_.getName).toSeq
        }
      }
    }
  }

  def file(gitRepo: String, version: Option[String], file: String): Future[String] = {
    val baseDir = new File(cacheDir.toFile, gitRepo)

    def readfile = Source.fromFile(new File(baseDir, file)).mkString

    if (!baseDir.exists()) {
      // clone the repo
      gitUrl(gitRepo).flatMap { url =>
        Future.fromTry {
          Try {
            val clone = org.eclipse.jgit.api.Git.cloneRepository()
              .setURI(url)
              .setDirectory(baseDir)

            version.foreach(clone.setBranch)

            clone.call()

            readfile
          }
        }
      }
    }
    else {
      // checkout the version
      val checkout = org.eclipse.jgit.api.Git.open(baseDir).checkout()

      version.fold(checkout.setName("origin/master"))(checkout.setName)

      checkout.call()

      Future.successful(readfile)
    }
  }


  def tar(url: String): Future[InputStream] = {
    // apply .npmignore
    // ignore .git
    // create tar

    /*
    Future.fromTry {
      Try {
        val url = new URL(s"$BASE_URL/$packageName/-/$packageName-$version.tgz")
        val inputStream = url.openConnection().getInputStream
        val gzipInputStream = new GZIPInputStream(inputStream)
        gzipInputStream
      }
    }
    */
    ???
  }

}

object Git {
  def apply(implicit ec: ExecutionContext, ws: WSClient) = new Git()
}
