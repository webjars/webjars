package utils

import io.lemonlabs.uri.AbsoluteUrl

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class Maven @Inject() (git: Git, semVer: SemVer) (implicit ec: ExecutionContext) {

  def convertNpmDependenciesToMaven(dependencies: Map[String, String]): Future[Map[String, String]] = {
    val maybeMavenDeps = dependencies.map { case (name, versionOrUrl) =>

      val urlTry = AbsoluteUrl.parseTry(versionOrUrl).filter { url =>
        !url.path.toString().endsWith(".git") && url.scheme != "git"
      }

      val nameAndVersionFuture = urlTry.map { url =>
        if (url.path.toString().contains("/tarball/")) {
          val host = url.host.toString().replaceAll("[^\\w\\d]", "-")
          val parts = url.path.toString().split("\\/tarball\\/")
          val path = parts(0).replaceAll("[^\\w\\d]", "-")
          val artifactId = host + path
          Future.successful(artifactId -> parts(1))
        }
        else {
          Future.failed(new Exception(s"Could not get a version from the dependency string: $url"))
        }
      }.getOrElse {
        if (git.isGit(versionOrUrl)) {
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
      }

      nameAndVersionFuture.flatMap { case (artifactId, version) =>
        if (version.contains("#")) {
          Future.failed(new Exception(s"Invalid version specified in dependency: $name $versionOrUrl"))
        }
        else {
          semVer.validRange(version).flatMap { maybeRange =>
            maybeRange.fold(Future.successful(artifactId -> version)) { range =>
              Future.fromTry(SemVer.toMaven(range)).map(artifactId -> _)
            }
          }
        }
      }
    }

    Future.sequence(maybeMavenDeps).map(_.toMap)
  }

}
