package utils

import java.net.URL

import javax.inject.Inject

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Maven @Inject() (git: Git) (implicit ec: ExecutionContext) {

  def convertNpmBowerDependenciesToMaven(dependencies: Map[String, String]): Future[Map[String, String]] = {
    val maybeMavenDeps = dependencies.map { case (name, versionOrUrl) =>

      val urlTry = Try(new URL(versionOrUrl)).filter(!_.getPath.endsWith(".git"))

      val nameAndVersionFuture = if (urlTry.isSuccess) {
        // a tgz which we currenty assume uses a github path syntax
        val url = urlTry.get

        if (url.getPath.contains("/tarball/")) {
          val host = url.getHost.replaceAll("[^\\w\\d]", "-")
          val parts = url.getPath.split("\\/tarball\\/")
          val path = parts(0).replaceAll("[^\\w\\d]", "-")
          val artifactId = host + path
          Future.successful(artifactId -> parts(1))
        }
        else {
          Future.failed(new Exception(s"Could not get a version from the dependency string: $url"))
        }
      }
      else if (git.isGit(versionOrUrl)) {
        val (url, maybeVersion) = if (versionOrUrl.contains("#")) {
          val parts = versionOrUrl.split("#")
          (parts.head, parts.lastOption)
        }
        else {
          (versionOrUrl, None)
        }

        git.artifactId(url).map(_.toLowerCase).flatMap { artifactId =>
          maybeVersion.fold {
            git.versions(url).flatMap { versions =>
              versions.headOption.fold {
                // could not get a tagged version so the latest commit instead
                git.versionsOnBranch(url, "master").flatMap { commits =>
                  commits.headOption.fold {
                    Future.failed[(String, String)](new Exception(s"The dependency definition $name -> $versionOrUrl was not valid because it looked like a git repo reference but no version was specified."))
                  } { latestCommit =>
                    Future.successful(artifactId -> s"0.0.0-$latestCommit")
                  }
                }
              } { latestVersion =>
                Future.successful(artifactId -> latestVersion)
              }
            }
          } { version =>
            Future.successful(artifactId -> version)
          }
        }
      }
      else {
        git.artifactId(name).map(_ -> versionOrUrl)
      }

      nameAndVersionFuture.flatMap { case (artifactId, version) =>
        if (version.contains("#")) {
          Future.failed(new Exception(s"Invalid version specified in dependency: $name $versionOrUrl"))
        }
        else {
          val maybeMavenVersion = SemVer.convertSemVerToMaven(version).map(artifactId -> _)
          Future.fromTry(maybeMavenVersion)
        }
      }
    }

    Future.sequence(maybeMavenDeps).map(_.toMap)
  }

}
