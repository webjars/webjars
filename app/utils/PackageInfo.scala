package utils

import java.net.URI

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util.Try

case class PackageInfo(name: String, version: String, homepage: String, sourceUrl: String, sourceConnectionUrl: String, issuesUrl: String, licenses: Seq[String], dependencies: Map[String, String]) {

  lazy val sourceUri: Try[URI] = Try { new URI(sourceUrl) }
  lazy val gitHubOrg: Try[String] = sourceUri.map(_.getPath.split("/")(1))
  lazy val gitHubRepo: Try[String] = sourceUri.map(_.getPath.split("/")(2).stripSuffix(".git"))
  lazy val gitHubOrgRepo: Try[String] = {
    for {
      org <- gitHubOrg
      repo <- gitHubRepo
    } yield s"$org/$repo"
  }
  lazy val gitHubHome: Try[String] = gitHubOrgRepo.map(orgRepo => s"https://github.com/$orgRepo")

}

object PackageInfo {
  implicit def jsonWrites: Writes[PackageInfo] = Json.writes[PackageInfo]
}