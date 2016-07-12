package utils

import models.WebJarCatalog
import play.api.test._
import utils.MavenCentral.UnavailableException

class MavenCentralSpec extends PlaySpecification {

  "fetchWebJars" should {
    "fail when the searchGroupUrl does not return JSON" in new WithServer(port = testServerPort, app = FakeApplication(additionalConfiguration = Map("webjars.searchGroupUrl" -> s"http://localhost:$testServerPort/asdf"))) {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      await(mavenCentral.fetchWebJars(WebJarCatalog.CLASSIC)) should throwA[UnavailableException]
    }
    "work normally" in new WithApplication() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val webJars = await(mavenCentral.fetchWebJars(WebJarCatalog.NPM))
      webJars.foldLeft(0)(_ + _.versions.size) should beEqualTo (50)
    }
  }

}
