package utils

import scala.concurrent.{ExecutionContext, Future}

class MavenUtil(implicit ec: ExecutionContext, git: GitUtil) {

  def convertNpmBowerDependenciesToMaven(dependencies: Map[String, String], latestVersion: Option[String] = None): Future[Map[String, String]] = {
    val maybeMavenDeps = dependencies.map { case (name, versionOrUrl) =>

      val ungitNameAndVersionFuture = if (git.isGit(versionOrUrl)) {
        val (url, maybeVersion) = if (versionOrUrl.contains("#")) {
          val parts = versionOrUrl.split("#")
          (parts.head, parts.lastOption)
        }
        else {
          (versionOrUrl, None)
        }

        git.artifactId(url).flatMap { artifactId =>
          maybeVersion.orElse(latestVersion).fold {
            Future.failed[(String, String)](new Exception(s"The dependency definition $name -> $versionOrUrl was not valid because it looked like a git repo reference but no version was specified."))
          } { version =>
            Future.successful(artifactId -> version)
          }
        }
      }
      else {
        Future.successful(name -> versionOrUrl)
      }

      ungitNameAndVersionFuture.flatMap { case (artifactId, version) =>
        val maybeMavenVersion = SemVerUtil.convertSemVerToMaven(version)
        maybeMavenVersion.fold {
          Future.failed[(String, String)](new Exception(s"Could not convert NPM / Bower version to Maven for: $name $versionOrUrl"))
        } { mavenVersion =>
          Future.successful(artifactId -> mavenVersion)
        }
      }
    }

    Future.sequence(maybeMavenDeps).map(_.toMap)
  }

}

object MavenUtil {
  def apply(implicit ec: ExecutionContext, git: GitUtil) = new MavenUtil()
}