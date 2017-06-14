package controllers

import models.{WebJar, WebJarVersion}
import play.api.http.{ContentTypes, HeaderNames, Status}
import play.api.test.{FakeApplication, FakeRequest, PlaySpecification, WithApplication}

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

  "searchWebJars" should {
    "work" in new WithApplication {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)
      val resultFuture = applicationController.searchWebJars("jquery", List("org.webjars"))(request)

      status(resultFuture) must beEqualTo (Status.OK)

      (contentAsJson(resultFuture) \\ "artifactId").map(_.as[String]) must contain ("jquery")
    }
    "work when stats can't be fetched" in new WithApplication(app = FakeApplication(additionalConfiguration = Map("oss.username" -> s"asdf"))) {
      val applicationController = app.injector.instanceOf[Application]

      val request = FakeRequest().withHeaders(HeaderNames.ACCEPT -> ContentTypes.JSON)
      val resultFuture = applicationController.searchWebJars("jquery", List("org.webjars"))(request)

      (contentAsJson(resultFuture) \\ "artifactId").map(_.as[String]) must contain ("jquery")
    }
  }

}
