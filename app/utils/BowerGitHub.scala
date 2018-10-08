package utils

import java.io.InputStream
import javax.inject.Inject

import org.eclipse.jgit.api.errors.RefNotFoundException
import play.api.libs.concurrent.Futures
import play.api.libs.json.Json
import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}

class BowerGitHub @Inject() (ws: WSClient, git: Git, gitHub: GitHub, maven: Maven)(implicit ec: ExecutionContext, futures: Futures)
  extends Bower(ws, git, gitHub, maven)(ec, futures) {

  override val name: String = "BowerGitHub"

  override val groupIdQuery: String = "org.webjars.bowergithub.*"

  override def includesGroupId(groupId: String): Boolean = groupId.startsWith("org.webjars.bowergithub.")

  override def groupId(nameOrUrlish: String, version: String): Future[String] = lookup(nameOrUrlish, version).flatMap { url =>
    GitHub.gitHubUrl(url).fold(Future.failed[String], { gitHubUrl =>
      gitHub.currentUrls(gitHubUrl).flatMap { case (currentGitHubUrl, _, _) =>
          GitHub.maybeGitHubOrg(Some(currentGitHubUrl)).fold {
            Future.failed[String](new Exception(s"Could not determine GitHub org from $currentGitHubUrl"))
          } { org =>
            Future.successful("org.webjars.bowergithub." + org.toLowerCase)
          }
      }
    })
  }

  override def artifactId(nameOrUrlish: String, version: String): Future[String] = lookup(nameOrUrlish, version).flatMap { url =>
    GitHub.gitHubUrl(url).fold(Future.failed[String], { gitHubUrl =>
      gitHub.currentUrls(gitHubUrl).flatMap { case (currentGitHubUrl, _, _) =>
        GitHub.maybeGitHubRepo(Some(currentGitHubUrl)).fold {
          Future.failed[String](new Exception(s"Could not determine GitHub repo from $currentGitHubUrl"))
        } { repo =>
          Future.successful(repo.toLowerCase)
        }
      }
    })
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    Future.sequence {
      dependencies.map(bowerToMaven).toSet
    }
  }

  override def pathPrefix(nameOrUrlish: String, releaseVersion: String, packageInfo: PackageInfo): Future[String] = {
    Future.successful(s"${packageInfo.name}/")
  }

  override def excludes(nameOrUrlish: String, version: String): Future[Set[String]] = {
    lookup(nameOrUrlish, version).flatMap { url =>
      val bowerJsonFuture = git.file(url.toURI, Some(version), "bower.json").recoverWith {
        // try with a "v" version prefix
        case _: RefNotFoundException if !version.startsWith("v") => git.file(url.toURI, Some("v" + version), "bower.json")
      }

      bowerJsonFuture.map { bowerJson =>
        val json = Json.parse(bowerJson)
        (json \ "ignore").asOpt[Set[String]].getOrElse(Set.empty[String])
      }
    }
  }

  override def archive(packageNameOrGitRepo: String, version: String): Future[InputStream] = {
    lookup(packageNameOrGitRepo, version).flatMap { url =>
      super.archive(url.toString, version).recoverWith {
        // try with a "v" version prefix
        case _: RefNotFoundException if !version.startsWith("v") => super.archive(url.toString, "v" + version)
      }
    }
  }

}
