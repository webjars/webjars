package utils

import org.joda.time.DateTime
import play.api.Configuration
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import utils.MavenCentral.UnavailableException

class MavenCentralSpec extends PlaySpecification {

  "fetchWebJars" should {
    "fail when the search-url does not return JSON" in new WithServer(port = testServerPort, app = GuiceApplicationBuilder(configuration = Configuration("mavencentral.search-url" -> s"http://localhost:$testServerPort/asdf")).build()) {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val classic = app.injector.instanceOf[Classic]
      await(mavenCentral.fetchWebJars(classic)) should throwA[UnavailableException]
    }
    "work normally" in new WithApplication() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val npm = app.injector.instanceOf[NPM]
      val webJars = await(mavenCentral.fetchWebJars(npm))
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
        val classic = app.injector.instanceOf[Classic]
        val statsClassic = await(mavenCentral.getStats(classic, new DateTime(2016, 1, 1, 1, 1)))
        statsClassic.head should beEqualTo("org.webjars", "jquery", 45947)

        val bowerWebJars = app.injector.instanceOf[BowerGitHub]
        val statsBowerWebJars = await(mavenCentral.getStats(bowerWebJars, new DateTime(2018, 1, 1, 1, 1)))
        statsBowerWebJars should contain(("org.webjars.bowergithub.polymerelements", "gold-zip-input", 10))
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
        mostDownloaded.head should beEqualTo("org.webjars", "jquery", 45947)
      }
    }
  }

}
