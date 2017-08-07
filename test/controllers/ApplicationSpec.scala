package controllers

import models.{WebJar, WebJarVersion}
import play.api.Configuration
import play.api.http.{ContentTypes, HeaderNames, Status}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}

import scala.util.Random


class ApplicationSpec extends PlaySpecification {

  "sortedWebJars" should {
    "work" in new WithApplication {
      val applicationController = app.injector.instanceOf[Application]

      val counts = Random.shuffle(
        Seq(
          ("foo", "foo", 3),
          ("foo", "bar", 2),
          ("foo", "baz", 1)
        )
      )

      val webJars = Random.shuffle(
        Seq(
          WebJar("foo", "foo", "foo", "foo", List.empty[WebJarVersion]),
          WebJar("foo", "bar", "foo", "foo", List.empty[WebJarVersion]),
          WebJar("foo", "baz", "foo", "foo", List.empty[WebJarVersion]),
          WebJar("foo", "blah", "foo", "foo", List.empty[WebJarVersion])
        )
      )

      val sorted = applicationController.sortedWebJars(counts, webJars)

      sorted(0).artifactId must beEqualTo ("foo")
      sorted(1).artifactId must beEqualTo ("bar")
      sorted(2).artifactId must beEqualTo ("baz")
      sorted(3).artifactId must beEqualTo ("blah")
    }
  }
  "sortedMostPopularWebJars" should {
    "only include the max number" in new WithApplication {
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
    "work" in new WithApplication {
      if (app.configuration.getOptional[String]("oss.username").isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val applicationController = app.injector.instanceOf[Application]

        val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)
        val resultFuture = applicationController.searchWebJars("jquery", List("org.webjars"))(request)

        status(resultFuture) must beEqualTo(Status.OK)

        // todo: this test flaps due to ordering of maven central results
        (contentAsJson(resultFuture) \\ "artifactId").map(_.as[String]) must contain("jquery-form")
      }
    }
    "work when stats can't be fetched" in new WithApplication(app = GuiceApplicationBuilder(configuration = Configuration("oss.username" -> "asdf", "oss.password" -> "asdf")).build()) {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)
      val resultFuture = applicationController.searchWebJars("jquery", List("org.webjars"))(request)

      // todo: this test flaps due to ordering of maven central results
      (contentAsJson(resultFuture) \\ "artifactId").map(_.as[String]) must contain("jquery-form")
    }
  }

}
