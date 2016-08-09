package utils

import models.WebJarCatalog
import org.joda.time.DateTime
import play.api.test._
import utils.MavenCentral.UnavailableException

class MavenCentralSpec extends PlaySpecification {

  "fetchWebJars" should {
    "fail when the searchGroupUrl does not return JSON" in new WithServer(port = testServerPort, app = FakeApplication(additionalConfiguration = Map("webjars.searchGroupUrl" -> s"http://localhost:$testServerPort/asdf"))) {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      await(mavenCentral.fetchWebJars(WebJarCatalog.CLASSIC)) should throwA[UnavailableException]
    }
    "work normally" in new WithApplication() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val webJars = await(mavenCentral.fetchWebJars(WebJarCatalog.NPM))
      webJars.foldLeft(0)(_ + _.versions.size) should beEqualTo (50)
    }
  }

  "getStats" should {
    "get the stats for a given date" in new WithApplication() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val stats = await(mavenCentral.getStats(WebJarCatalog.CLASSIC, new DateTime(2016, 1, 1, 1, 1)))
      stats.head should beEqualTo (WebJarCatalog.CLASSIC, "jquery", 45947)
    }
  }

  "mostDownloaded" should {
    "get the 20 most downloaded for each catalog" in new WithApplication() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val mostDownloaded = await(mavenCentral.mostDownloaded(new DateTime(2016, 1, 1, 1, 1), 20))
      mostDownloaded.size should beEqualTo (60)
      mostDownloaded.head should beEqualTo (WebJarCatalog.CLASSIC, "jquery", 45947)
    }
  }

}
