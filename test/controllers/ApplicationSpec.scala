package controllers

import models.{WebJar, WebJarType, WebJarVersion}
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
          WebJar("foo", "foo", "foo", "foo", "foo", List.empty[WebJarVersion]),
          WebJar("foo","foo", "bar", "foo", "foo", List.empty[WebJarVersion]),
          WebJar("foo","foo", "baz", "foo", "foo", List.empty[WebJarVersion]),
          WebJar("foo","foo", "blah", "foo", "foo", List.empty[WebJarVersion])
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

        val webJars = contentAsJson(applicationController.webJarList("org.webjars")(request)).as[Seq[WebJar]]

        val possibleMatches = webJars.filter(_.artifactId.toLowerCase.contains("openui5"))

        val resultFuture = applicationController.searchWebJars("openui5", List("org.webjars"))(request)

        status(resultFuture) must beEqualTo(Status.OK)

        contentAsJson(resultFuture).as[Seq[WebJar]] must containTheSameElementsAs(possibleMatches)
      }
    }
    "work when stats can't be fetched" in new WithApplication(app = GuiceApplicationBuilder(configuration = Configuration("oss.username" -> "asdf", "oss.password" -> "asdf")).build()) {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)

      val webJars = contentAsJson(applicationController.webJarList("org.webjars")(request)).as[Seq[WebJar]]

      val possibleMatches = webJars.filter(_.artifactId.toLowerCase.contains("openui5"))

      val resultFuture = applicationController.searchWebJars("openui5", List("org.webjars"))(request)

      contentAsJson(resultFuture).as[Seq[WebJar]] must containTheSameElementsAs(possibleMatches)
    }
  }

  "create" should {
    "work" in new WithApplication {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest()

      val result = applicationController.create("bowergithub", "jquery", "3.3.0")(request)

      status(result) must beEqualTo (Status.OK)
      contentAsBytes(result).length must beEqualTo (460313)
    }
  }

}
