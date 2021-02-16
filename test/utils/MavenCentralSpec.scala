package utils

import akka.util.Timeout
import org.apache.commons.io.IOUtils
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import play.api.{Configuration, Environment}
import utils.MavenCentral.GAV

import java.net.{URI, URL}
import scala.concurrent.duration._

class MavenCentralSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  def appWithLocalMavenSearch = GuiceApplicationBuilder(configuration = Configuration("mavencentral.search-url" -> s"http://localhost:$testServerPort/asdf"))
    .overrides(bind[Memcache].to[MemcacheMock])
    .build()

  class WithApp extends WithApplication(_.overrides(bind[Memcache].to[MemcacheMock]))

  /*
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
  */

  "deploy" should {
    "asc" in new WithApp() {
      if (app.configuration.getOptional[String]("oss.gpg-key").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        mavenCentral.asc("foo".getBytes) must beSome
      }
    }
    "create, upload, close, drop" in new WithApp() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val environment = app.injector.instanceOf[Environment]

        val jar = environment.resourceAsStream("foo.jar").map { inputStream =>
          val fileBytes = IOUtils.toByteArray(inputStream)
          inputStream.close()
          fileBytes
        }.get

        val gitUri = new URI("https://githib.com/webjars/webjars.git")
        val sourceUrl = new URL("https://github.com/webjars/webjars")
        val version = "0.0.1" // Instant.now.getEpochSecond.toString
        val licenses = Map("MIT" -> "https://opensource.org/licenses/MIT")

        val packageInfo = PackageInfo("Test WebJar", version, None, gitUri, None, Seq.empty, Map.empty, Map.empty)

        val gav = GAV("org.webjars", "_test", version)

        val pom = templates.xml.pom(gav.groupId, gav.artifactId, gav.version, packageInfo, sourceUrl, Set.empty, Set.empty, licenses).toString()

        val stagingRepo = await(mavenCentral.createStaging("test create"))
        stagingRepo.description mustEqual "test create"

        await(mavenCentral.uploadStaging(stagingRepo, gav, pom, jar))

        await(mavenCentral.closeStaging(stagingRepo, "test close")) must not(throwAn[Exception])

        //await(mavenCentral.promoteStaging(stagingProfile, "test promote")) must not(throwAn[Exception])

        await(mavenCentral.dropStaging(stagingRepo, "test drop")) must not(throwAn[Exception])
      }
    }
  }

}