package utils

import java.io.InputStream
import java.net.URI

import scala.concurrent.Future

case class DeployableMock() extends Deployable {
  override def groupId(nameOrUrlish: String, version: String): Future[String] = ???
  override def archive(nameOrUrlish: String, version: String): Future[InputStream] = ???
  override def artifactId(nameOrUrlish: String, version: String): Future[String] = ???
  override def info(nameOrUrlish: String, maybeVersion: Option[String], maybeSourceUri: Option[URI]): Future[PackageInfo] = ???
  override def excludes(nameOrUrlish: String, version: String): Future[Set[String]] = ???
  override lazy val metadataFile: String = "foo.json"
  override lazy val contentsInSubdir: Boolean = ???
  override def includesGroupId(groupId: String): Boolean = ???
  override lazy val groupIdQuery: String = ???
  override lazy val name: String = ???
  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = ???
  override def pathPrefix(nameOrUrlish: String, releaseVersion: String, packageInfo: PackageInfo): Future[String] = ???
  override def versions(nameOrUrlish: String): Future[Set[String]] = ???
  override def parseDep(dep: (String, String)): (String, String) = ???
}
