package utils

import org.joda.time.DateTime
import play.api.Configuration
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import utils.MavenCentral.UnavailableException

class MavenCentralSpec extends PlaySpecification {

  "fetchWebJars" should {
    "fail when the searchGroupUrl does not return JSON" in new WithServer(port = testServerPort, app = GuiceApplicationBuilder(configuration = Configuration("webjars.searchGroupUrl" -> s"http://localhost:$testServerPort/asdf")).build()) {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      await(mavenCentral.fetchWebJars(Classic.groupId)) should throwA[UnavailableException]
    }
    "work normally" in new WithApplication() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val webJars = await(mavenCentral.fetchWebJars(NPM.groupId))
      webJars.foldLeft(0)(_ + _.versions.size) should beEqualTo (50)
    }
  }

  "getStats" should {
    "get the stats for a given date" in new WithApplication() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val stats = await(mavenCentral.getStats(Classic.groupId, new DateTime(2016, 1, 1, 1, 1)))
        stats.head should beEqualTo(Classic.groupId, "jquery", 45947)
      }
    }
  }

  "mostDownloaded" should {
    "get the 20 most downloaded for each catalog" in new WithApplication() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val mostDownloaded = await(mavenCentral.mostDownloaded(new DateTime(2016, 1, 1, 1, 1), 20))
        mostDownloaded.size should beEqualTo(60)
        mostDownloaded.head should beEqualTo(NPM.groupId, "validate.js", 2901)
      }
    }
  }

}
