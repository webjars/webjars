package utils

import org.eclipse.jgit.api.{Git => GitApi}
import play.api.http.{HeaderNames, Status}
import play.api.libs.ws.WSClient
import utils.Deployable.Version

import java.io.{File, InputStream}
import java.net.{URI, URL}
import java.nio.charset.CodingErrorAction
import java.nio.file.{Files, Path}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.io.{Codec, Source}
import scala.jdk.CollectionConverters._
import scala.reflect.io.Directory
import scala.util.{Try, Using}

class Git @Inject() (ws: WSClient) (implicit ec: ExecutionContext) {

  val cacheDir: Path = Files.createTempDirectory("git")
  cacheDir.toFile.deleteOnExit()

  def isGit(packageNameOrGitRepo: String): Boolean = {
    packageNameOrGitRepo.contains("/") && !packageNameOrGitRepo.startsWith("@")
  }

  def resolveRedir(httpUrl: String): Future[String] = {
    ws.url(httpUrl).withFollowRedirects(false).head().flatMap { response =>
      response.status match {
        case Status.MOVED_PERMANENTLY | Status.FOUND =>
          // todo: max redirects
          response.header(HeaderNames.LOCATION).fold(Future.failed[String](new Exception("Could not get redir location"))) { location =>
            val redirUrl = if (location.startsWith("/")) {
              // relative url
              val url = new URL(httpUrl)
              url.getProtocol + "://" + url.getHost + location
            }
            else {
              location
            }

            // keep resolving until there is an OK
            resolveRedir(redirUrl)
          }
        case Status.OK =>
          Future.successful(httpUrl)
        case _ =>
          Future.failed(new Exception(s"Could not get HEAD for url: $httpUrl"))
      }
    }
  }

  def gitUrl(gitRepo: String): Future[String] = {
    val resolvedUrl = if (gitRepo.contains("://")) {
      gitRepo
    }
    else if (gitRepo.contains("github:")) {
      gitRepo.replace("github:", "https://github.com/")
    }
    else {
      // infer github
      s"https://github.com/$gitRepo"
    }

    val jgitReadyUrl = resolvedUrl.replace("git+", "")

    if (jgitReadyUrl.startsWith("http")) {
      resolveRedir(jgitReadyUrl)
    }
    else {
      Future.successful(jgitReadyUrl)
    }
  }

  // converts git urls to unique artifactId's
  def artifactId(nameOrUrlish: String): Future[String] = {
    if (isGit(nameOrUrlish)) {
      gitUrl(nameOrUrlish).flatMap { gitUrl =>
        Future.fromTry {
          Try {
            val uri = new URI(gitUrl.stripSuffix(".git"))

            val host = uri.getHost.replaceAll("[^\\w\\d]", "-")
            val path = uri.getPath.replaceAll("[^\\w\\d]", "-")

            host + path
          }
        }
      }
    }
    else {
      val encoded = nameOrUrlish.replace("@", "").replace("/", "__")
      Future.successful(encoded)
    }
  }

  def versions(gitRepo: String): Future[Set[String]] = {
    gitUrl(gitRepo).flatMap { url =>
      Future.fromTry {
        Try {
          val tags = GitApi.lsRemoteRepository()
            .setRemote(url)
            .setTags(true)
            .call()

          tags.asScala.map(_.getName.stripPrefix("refs/tags/")).toSet
        }
      }
    }
  }

  def versionsOnBranch(gitRepo: String, branch: String): Future[Seq[String]] = {
    gitUrl(gitRepo).flatMap { _ =>
      cloneOrCheckout(gitRepo, s"origin/$branch").flatMap { baseDir =>
        Future.fromTry {
          Try {
            val commits = GitApi.open(baseDir).log().call()
            commits.asScala.toSeq.map(_.getId.abbreviate(10).name())
          }
        }
      }
    }
  }

  // todo: only clone the specified version to speed things up
  def cloneOrCheckout(gitRepo: String, version: Version, retry: Boolean = true): Future[File] = {
    gitUrl(gitRepo).flatMap { url =>
      val baseDir = new File(cacheDir.toFile, url)

      val cloneOrPullFuture = if (!baseDir.exists()) {
        // clone the repo
        Future.fromTry {
          Try {
            val clone = GitApi.cloneRepository()
              .setURI(url)
              .setDirectory(baseDir)
              .setCloneAllBranches(true)
              .setNoCheckout(true)

            clone.call()

            baseDir
          }
        }
      }
      else {
        Future.fromTry {
          Try {
            val pull = GitApi.open(baseDir).fetch().setRemote("origin")
            pull.call()
            baseDir
          }
        }
      }

      val checkoutFuture = cloneOrPullFuture.flatMap { _ =>
        Future.fromTry {
          Try {
            // checkout the version
            val checkout = GitApi.open(baseDir).checkout()

            checkout.setName(version)
            checkout.setForced(true)

            checkout.call()

            baseDir
          }
        }
      }

      checkoutFuture.recoverWith {
        case t: Throwable =>
          new Directory(baseDir).deleteRecursively()
          if (retry) {
            cloneOrCheckout(gitRepo, version, retry = false)
          }
          else {
            Future.failed(t)
          }
      }
    }
  }

  def file(uri: URI, version: Version, fileName: String): Future[String] = file(uri.toString, version, fileName)

  def file(gitRepo: String, tagCommitOrBranch: Version, fileName: String): Future[String] = {
    cloneOrCheckout(gitRepo, tagCommitOrBranch).flatMap { baseDir =>
      Future.fromTry {
        val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
        Using(Source.fromFile(new File(baseDir, fileName))(decoder))(_.mkString)
      }
    }
  }

  def tar(gitRepo: String, version: Version, excludes: Set[String]): Future[InputStream] = {
    cloneOrCheckout(gitRepo, version).flatMap { baseDir =>
      // todo: apply .npmignore

      val resolvedExcludes = excludes.map(new File(baseDir, _))

      val allExcludes = Set(new File(baseDir, ".git")) ++ resolvedExcludes

      Future.fromTry {
        ArchiveCreator.tarDir(baseDir, allExcludes)
      }
    }
  }

}