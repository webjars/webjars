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

  def bowerToMaven(keyValue: (String, String)): Future[(String, String, String)] = {
    val (key, value) = keyValue

    def convertSemVerToMaven(version: String): Future[String] = {
      SemVer.convertSemVerToMaven(version).fold {
        Future.failed[String](new Exception(s"Could not convert version '$version' to Maven form"))
      } (Future.successful)
    }

    if (value.contains("/")) {
      val urlish = value.takeWhile(_ != '#')
      val version = value.stripPrefix(urlish).stripPrefix("#").stripPrefix("v")
      for {
        groupId <- groupId(urlish)
        artifactId <- artifactId(urlish)
        version <- convertSemVerToMaven(version)
      } yield (groupId, artifactId, version)
    }
    else {
      for {
        groupId <- groupId(key)
        artifactId <- artifactId(key)
        version <- convertSemVerToMaven(value.stripPrefix("v"))
      } yield (groupId, artifactId, version)
    }
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    Future.sequence {
      dependencies.map(bowerToMaven).toSet
    }
  }

  override def pathPrefix(packageInfo: PackageInfo): String = {
    s"${packageInfo.name}/"
  }

}
