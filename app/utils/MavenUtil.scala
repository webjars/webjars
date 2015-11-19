package utils

import scala.concurrent.{ExecutionContext, Future}

class MavenUtil(implicit ec: ExecutionContext, git: GitUtil) {

  def convertNpmBowerDependenciesToMaven(dependencies: Map[String, String]): Future[Map[String, String]] = {
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