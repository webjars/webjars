package utils

import javax.inject.Inject

import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}


class BowerGitHub @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector, gitHub: GitHub, maven: Maven)(implicit ec: ExecutionContext)
  extends Bower(ws, git, licenseDetector, gitHub, maven)(ec) {

  override val name: String = "BowerGitHub"

  override val groupIdQuery: String = "org.webjars.bowergithub.*"

  override def includesGroupId(groupId: String): Boolean = groupId.startsWith("org.webjars.bowergithub.")

  override def groupId(nameOrUrlish: String): Future[String] = lookup(nameOrUrlish).flatMap { url =>
    GitHub.maybeGitHubOrg(Some(url)).fold {
      Future.failed[String](new Exception(s"Could not determine GitHub org from $url"))
    } { org =>
      Future.successful("org.webjars.bowergithub." + org.toLowerCase)
    }
  }

  override def artifactId(nameOrUrlish: String): Future[String] = lookup(nameOrUrlish).flatMap { url =>
    GitHub.maybeGitHubRepo(Some(url)).fold {
      Future.failed[String](new Exception(s"Could not determine GitHub repo from $url"))
    } { repo =>
      Future.successful(repo.toLowerCase)
    }
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    Future.sequence {
      dependencies.map { case (nameOrUrlish, version) =>
        for {
          groupId <- groupId(nameOrUrlish)
          artifactId <- artifactId(nameOrUrlish)
          version <- SemVer.convertSemVerToMaven(version).fold(Future.failed[String](new Exception(s"Could not convert version '$version' to Maven form")))(Future.successful)
        } yield (groupId, artifactId, version)
      }.toSet
    }
  }

  override def pathPrefix(packageInfo: PackageInfo): String = {
    s"${packageInfo.name}/"
  }

}
