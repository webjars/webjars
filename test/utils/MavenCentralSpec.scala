package utils

import akka.util.Timeout
import models.{WebJar, WebJarType}
import org.apache.commons.io.IOUtils
import org.joda.time.DateTime
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import play.api.{Configuration, Environment}
import utils.MavenCentral.{GAV, StagedRepo}

import java.io.FileNotFoundException
import java.net.{URI, URL}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.xml.Elem

class MavenCentralSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  def appWithLocalMavenSearch = GuiceApplicationBuilder(configuration = Configuration("mavencentral.search-url" -> s"http://localhost:$testServerPort/asdf"))
    .overrides(bind[Memcache].to[MemcacheMock])
    .overrides(bind[MavenCentral].to[MavenCentralLive])
    .build()

  class WithApp extends WithApplication(_.overrides(bind[Memcache].to[MemcacheMock]))

  "fetchWebJars" should {
    "fail when the search-url does not return JSON" in new WithServer(port = testServerPort, app = appWithLocalMavenSearch) {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val classic = app.injector.instanceOf[Classic]
      await(mavenCentral.fetchWebJars(classic)) should throwA[MavenCentral.UnavailableException]
    }
    "work normally" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val npm = app.injector.instanceOf[NPM]
      val webJars = await(mavenCentral.fetchWebJars(npm))
      webJars.foldLeft(0)(_ + _.versions.size) should beGreaterThan (0)
    }

  }

  "webJarsSorted" should {
    "be ordered correctly" in new WithApp() {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val webJars = await(mavenCentral.webJarsSorted(new DateTime(2016, 1, 1, 1, 1)))
        webJars.head.artifactId must beEqualTo("jquery")
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
      if (
        app.configuration.getOptional[String]("oss.username").isEmpty ||
        app.configuration.getOptional[String]("oss.password").isEmpty ||
        app.configuration.getOptional[String]("oss.gpg-key").isEmpty
      ) {
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
        val licenses = Set[License](LicenseWithNameAndUrl("MIT", new URL("https://opensource.org/licenses/MIT")))

        val packageInfo = PackageInfo("Test WebJar", version, None, gitUri, None, Seq.empty, Map.empty, Map.empty, None)

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

class MavenCentralMock extends MavenCentral {
  override def fetchWebJars(webJarType: WebJarType): Future[List[WebJar]] = {
    Future.successful(List.empty)
  }

  // this makes it so the mock says the artifact has not already been deployed
  override def fetchPom(gav: GAV, maybeUrlPrefix: Option[String]): Future[Elem] = {
    Future.failed(new FileNotFoundException())
  }

  override def webJars(webJarType: WebJarType): Future[List[WebJar]] = {
    Future.successful(List.empty)
  }

  override def webJarsSorted(dateTime: DateTime): Future[List[WebJar]] = {
    Future.successful(List.empty)
  }

  override def createStaging(description: String): Future[MavenCentral.StagedRepo] = {
    Future.successful(StagedRepo("test", "test"))
  }

  override def uploadStaging(stagedRepo: MavenCentral.StagedRepo, gav: GAV, pom: String, jar: Array[Byte]): Future[Unit] = {
    Future.unit
  }

  override def closeStaging(stagedRepo: MavenCentral.StagedRepo, description: String): Future[Unit] = {
    Future.unit
  }

  override def promoteStaging(stagedRepo: MavenCentral.StagedRepo, description: String): Future[Unit] = {
    Future.unit
  }

  override def dropStaging(stagedRepo: MavenCentral.StagedRepo, description: String): Future[Unit] = {
    Future.unit
  }

  override def getStats(webJarType: WebJarType, dateTime: DateTime): Future[Map[(String, String), Port]] = {
    Future.successful(Map.empty)
  }

  override def asc(toSign: Array[Byte]): Option[String] = {
    None
  }
}