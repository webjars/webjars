package utils

import akka.util.Timeout
import org.joda.time.DateTime
import play.api.Configuration
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import utils.MavenCentral.UnavailableException

import scala.concurrent.duration._

class MavenCentralSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  def appWithLocalMavenSearch = GuiceApplicationBuilder(configuration = Configuration("mavencentral.search-url" -> s"http://localhost:$testServerPort/asdf"))
    .overrides(bind[Memcache].to[MemcacheMock])
    .build()

  class WithApp extends WithApplication(_.overrides(bind[Memcache].to[MemcacheMock]))

  "fetchWebJars" should {
    "fail when the search-url does not return JSON" in new WithServer(port = testServerPort, app = appWithLocalMavenSearch) {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val classic = app.injector.instanceOf[Classic]
      await(mavenCentral.fetchWebJars(classic)) should throwA[UnavailableException]
    }
    "work normally" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val npm = app.injector.instanceOf[NPM]
      val webJars = await(mavenCentral.fetchWebJars(npm))
      webJars.foldLeft(0)(_ + _.versions.size) should beGreaterThan (0)
    }
    "be ordered correctly" in new WithApp() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val classic = app.injector.instanceOf[Classic]
        val webJars = await(mavenCentral.fetchWebJars(classic, new DateTime(2016, 1, 1, 1, 1)))
        webJars.head.artifactId must beEqualTo("bootstrap")
      }
    }
  }

  "getStats" should {
    "get the stats for a given date" in new WithApp() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val classic = app.injector.instanceOf[Classic]
        val statsClassic = await(mavenCentral.getStats(classic, new DateTime(2016, 1, 1, 1, 1)))
        statsClassic(("org.webjars", "jquery")) should beEqualTo(45947)

        val bowerGitHub = app.injector.instanceOf[BowerGitHub]
        val bowerWebJars = await(mavenCentral.webJars(bowerGitHub))

        val statsBowerWebJars = await(mavenCentral.getStats(bowerGitHub, new DateTime(2019, 1, 1, 1, 1)))

        val ((groupId, _), downloads) = statsBowerWebJars.head
        downloads should be > 0

        bowerWebJars.find(_.groupId == groupId) should beSome
      }
    }
  }

  "deploy" should {
    "create, close, promote" in new WithApp() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]

        val stagingProfile = await(mavenCentral.createStaging("test create"))
        stagingProfile.description mustEqual "test create"

        await(mavenCentral.closeStaging(stagingProfile, "test close")) must not(throwAn[Exception])

        await(mavenCentral.promoteStaging(stagingProfile, "test promote")) must not(throwAn[Exception])
      }
    }
  }

}