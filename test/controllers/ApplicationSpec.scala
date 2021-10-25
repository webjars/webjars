package controllers

import akka.util.Timeout
import models.WebJar
import play.api.http.{ContentTypes, HeaderNames, Status}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}
import utils.{BowerGitHub, Memcache, MemcacheMock}

import scala.concurrent.duration._

class TestFetchConfig extends FetchConfig {
  override val timeout = 5.minutes
}

class ApplicationSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  val withOverrides = (gab: GuiceApplicationBuilder) => gab.overrides(bind[FetchConfig].to[TestFetchConfig])

  class WithApp extends WithApplication(withOverrides)

  "sortedMostPopularWebJars" should {
    "only include the max number" in new WithApp {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val applicationController = app.injector.instanceOf[Application]

        val sorted = await(applicationController.sortedMostPopularWebJars)

        sorted must not be empty

        val grouped = sorted.groupBy(_.groupId)

        grouped.getOrElse("org.webjars", Seq.empty[WebJar]).length must beLessThanOrEqualTo(applicationController.MAX_POPULAR_WEBJARS)
        grouped.getOrElse("org.webjars.npm", Seq.empty[WebJar]).length must beLessThanOrEqualTo(applicationController.MAX_POPULAR_WEBJARS)
        grouped.getOrElse("org.webjars.bower", Seq.empty[WebJar]).length must beLessThanOrEqualTo(applicationController.MAX_POPULAR_WEBJARS)
      }
    }
  }

  "searchWebJars" should {
    "work with a classic webjar" in new WithApp {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val applicationController = app.injector.instanceOf[Application]

        val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)

        val webJars = contentAsJson(applicationController.webJarList("org.webjars")(request)).as[Seq[WebJar]]

        val possibleMatches = webJars.filter(_.artifactId.toLowerCase.contains("openui5"))

        val resultFuture = applicationController.searchWebJars("openui5", List("org.webjars"))(request)

        status(resultFuture) must beEqualTo(Status.OK)

        contentAsJson(resultFuture).as[Seq[WebJar]] must containTheSameElementsAs(possibleMatches)

        // todo: verify that the ordering was correct (i.e. the stats worked)
      }
    }
    "work with a bowergithub webjar" in new WithApp {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val applicationController = app.injector.instanceOf[Application]
        val bowerGitHub = app.injector.instanceOf[BowerGitHub]

        val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)

        val webJars = contentAsJson(applicationController.webJarList(bowerGitHub.groupIdQuery)(request)).as[Seq[WebJar]]

        val possibleMatchesOnArtifact = webJars.filter(_.artifactId.toLowerCase.contains("polymer-tinymce"))

        val resultOnArtifactFuture = applicationController.searchWebJars("polymer-tinymce", List(bowerGitHub.groupIdQuery))(request)

        status(resultOnArtifactFuture) must beEqualTo(Status.OK)

        contentAsJson(resultOnArtifactFuture).as[Seq[WebJar]] must containTheSameElementsAs(possibleMatchesOnArtifact)

        val possibleMatchesOnGroup = webJars.filter(_.groupId.toLowerCase.contains("org.webjars.bowergithub.pagekit"))

        val resultOnGroupFuture = applicationController.searchWebJars("pagekit", List(bowerGitHub.groupIdQuery))(request)

        status(resultOnGroupFuture) must beEqualTo(Status.OK)

        contentAsJson(resultOnGroupFuture).as[Seq[WebJar]] must containTheSameElementsAs(possibleMatchesOnGroup)
      }
    }
    "work when stats can't be fetched" in new WithApplication(withOverrides.andThen(_.configure("oss.username" -> "asdf", "oss.password" -> "asdf")).andThen(_.overrides(bind[Memcache].to[MemcacheMock]))) { // use MemcacheMock otherwise the stats get pulled from memcache
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)

      val webJars = contentAsJson(applicationController.webJarList("org.webjars")(request)).as[Seq[WebJar]]

      val possibleMatches = webJars.filter(_.artifactId.toLowerCase.contains("openui5"))

      val resultFuture = applicationController.searchWebJars("openui5", List("org.webjars"))(request)

      contentAsJson(resultFuture).as[Seq[WebJar]] must containTheSameElementsAs(possibleMatches)
    }
  }

  "create" should {
    "work" in new WithApp {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest()

      val result = applicationController.create("bowergithub", "jquery", "3.3.0")(request)

      status(result) must beEqualTo (Status.OK)
      contentAsBytes(result).length must beEqualTo (464604)
    }
  }

  "versions" should {
    "be sorted correctly" in new WithApp {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest()

      val result = applicationController.packageVersions("npm", "https://github.com/jindw/xmldom.git", Some("master"))(request)

      val shas = contentAsJson(result).as[Seq[String]]
      shas.dropWhile(_ != "366159a76a").drop(1).head must beEqualTo ("0be2ae910a")
    }
  }

}