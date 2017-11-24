package utils

import javax.inject.Inject

import play.api.libs.ws._

import scala.concurrent.ExecutionContext


class BowerGitHub @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector, gitHub: GitHub)(ec: ExecutionContext)
  extends Bower(ws, git, licenseDetector, gitHub)(ec) {

  override val name: String = "BowerGitHub"

  override val groupIdQuery: String = "org.webjars.bowergithub.*"

  override def includesGroupId(groupId: String): Boolean = groupId.startsWith("org.webjars.bowergithub.")

  override def groupId(packageInfo: PackageInfo): Option[String] = packageInfo.maybeGitHubOrg.map { gitHubOrg =>
    s"org.webjars.bowergithub.${gitHubOrg.toLowerCase}"
  }

  override def artifactId(packageInfo: PackageInfo): Option[String] = packageInfo.maybeGitHubRepo.map { gitHubRepo =>
    gitHubRepo.toLowerCase
  }

}
