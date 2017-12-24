package utils

import javax.inject.Inject

import play.api.libs.ws._

import scala.concurrent.{ExecutionContext, Future}


class BowerGitHub @Inject() (ws: WSClient, git: Git, licenseDetector: LicenseDetector, gitHub: GitHub, maven: Maven)(ec: ExecutionContext)
  extends Bower(ws, git, licenseDetector, gitHub, maven)(ec) {

  override val name: String = "BowerGitHub"

  override val groupIdQuery: String = "org.webjars.bowergithub.*"

  override def includesGroupId(groupId: String): Boolean = groupId.startsWith("org.webjars.bowergithub.")

  // todo: if just given a name, we should lookup the repo in the Bower system instead of trusting the bower metadata
  override def groupId(nameOrUrlish: String): Future[String] = Future.failed(new NotImplementedError())

  // todo: if just given a name, we should lookup the repo in the Bower system instead of trusting the bower metadata
  override def artifactId(nameOrUrlish: String): Future[String] = Future.failed(new NotImplementedError())

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    Future.failed[Set[(String, String, String)]](new Exception("not implemented"))
  }

  override def pathPrefix(packageInfo: PackageInfo): String = {
    s"${packageInfo.name}/"
  }

}
