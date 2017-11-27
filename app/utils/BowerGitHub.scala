package utils

import javax.inject.Inject

import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}


class BowerGitHub @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector, gitHub: GitHub, maven: Maven)(ec: ExecutionContext)
  extends Bower(ws, git, licenseDetector, gitHub, maven)(ec) {

  override val name: String = "BowerGitHub"

  override val groupIdQuery: String = "org.webjars.bowergithub.*"

  override def includesGroupId(groupId: String): Boolean = groupId.startsWith("org.webjars.bowergithub.")

  override def groupId(packageInfo: PackageInfo): Option[String] = packageInfo.maybeGitHubOrg.map { gitHubOrg =>
    s"org.webjars.bowergithub.${gitHubOrg.toLowerCase}"
  }

  override def artifactId(packageInfo: PackageInfo): Option[String] = packageInfo.maybeGitHubRepo.map { gitHubRepo =>
    gitHubRepo.toLowerCase
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    Future.failed[Set[(String, String, String)]](new Exception("not implemented"))
  }

  override def pathPrefix(packageInfo: PackageInfo): String = {
    s"${packageInfo.name}/"
  }

}
