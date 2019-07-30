package utils

import java.net.{URI, URL}

import play.api.libs.json.{JsArray, JsValue, Json}

import scala.concurrent.Future

class BinTrayMock extends BinTray {
  override def createPackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUri: URI, websiteUrl: Option[URL], issueTrackerUrl: Option[URL], githubRepo: Option[String]): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def getPackages(subject: String, repo: String): Future[JsArray] = {
    Future.successful(
      Json.arr()
    )
  }

  override def getOrCreatePackage(subject: String, repo: String, name: String, desc: String, labels: Seq[String], licenses: Set[String], vcsUri: URI, websiteUrl: Option[URL], issueTrackerUrl: Option[URL], githubRepo: Option[String]): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def createVersion(subject: String, repo: String, packageName: String, version: String, description: String, vcsTag: Option[String]): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def createOrOverwriteVersion(subject: String, repo: String, packageName: String, version: String, description: String, vcsTag: Option[String]): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def deletePackage(subject: String, repo: String, name: String): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def uploadMavenArtifact(subject: String, repo: String, packageName: String, path: String, jarBytes: Array[Byte]): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def signVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def publishVersion(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }

  override def syncToMavenCentral(subject: String, repo: String, packageName: String, version: String): Future[JsValue] = {
    Future.successful(
      Json.obj()
    )
  }
}
