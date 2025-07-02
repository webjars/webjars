package controllers

import models.WebJar
import org.apache.pekko.util.Timeout
import play.api.http.{ContentTypes, HeaderNames, Status}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}
import utils.MavenCentral

import scala.concurrent.duration._

class TestFetchConfig extends FetchConfig {
  override val timeout = 5.minutes
}

class ApplicationSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  val limit = 5

  val withOverrides = (gab: GuiceApplicationBuilder) => gab.overrides(bind[FetchConfig].to[TestFetchConfig]).configure("mavencentral.limit" -> limit)

  class WithApp extends WithApplication(withOverrides)

  "sortedMostPopularWebJars" should {
    "only include the max number" in new WithApp {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val applicationController = app.injector.instanceOf[Application]

      val sorted = await(applicationController.sortedMostPopularWebJars)

      sorted must not be empty

      val grouped = sorted.groupBy(_.groupId)

      grouped.getOrElse("org.webjars", Seq.empty[WebJar]).length must beLessThanOrEqualTo(applicationController.MAX_POPULAR_WEBJARS)
      grouped.getOrElse("org.webjars.npm", Seq.empty[WebJar]).length must beLessThanOrEqualTo(applicationController.MAX_POPULAR_WEBJARS)
    }
  }

  "searchWebJars" should {
    "work with a classic webjar" in new WithApp {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
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

  "create" should {
    "work" in new WithApp {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest()

      val result = applicationController.create("npm", "jquery", "3.3.0")(request)

      status(result) must beEqualTo (Status.OK)
      contentAsBytes(result).length must beEqualTo (462901)
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
