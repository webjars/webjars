package utils

import java.io.InputStream
import java.net.URI

import play.api.libs.concurrent.Futures

import scala.concurrent.{ExecutionContext, Future}

case class DeployableMock() extends Deployable {
  override def groupId(nameOrUrlish: String, version: String): Future[String] = Future.failed(new NotImplementedError())
  override def archive(nameOrUrlish: String, version: String): Future[InputStream] = Future.failed(new NotImplementedError())
  override def artifactId(nameOrUrlish: String, version: String): Future[String] = Future.failed(new NotImplementedError())
  override def info(nameOrUrlish: String, maybeVersion: Option[String], maybeSourceUri: Option[URI]): Future[PackageInfo] = Future.failed(new NotImplementedError())
  override def excludes(nameOrUrlish: String, version: String): Future[Set[String]] = Future.failed(new NotImplementedError())
  override lazy val metadataFile: String = "foo.json"
  override lazy val contentsInSubdir: Boolean = false
  override def includesGroupId(groupId: String): Boolean = false
  override lazy val groupIdQuery: String = "nothing"
  override lazy val name: String = "nothing"
  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = Future.failed(new NotImplementedError())
  override def pathPrefix(nameOrUrlish: String, releaseVersion: String, packageInfo: PackageInfo): Future[String] = Future.failed(new NotImplementedError())
  override def versions(nameOrUrlish: String): Future[Set[String]] = Future.failed(new NotImplementedError())
  override def parseDep(dep: (String, String)): (String, String) = ("nothing", "nothing")
  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String])(implicit ec: ExecutionContext, futures: Futures) = Future.failed(new NotImplementedError())
}
