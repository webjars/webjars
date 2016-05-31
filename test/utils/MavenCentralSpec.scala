package utils

import models.WebJarCatalog
import play.api.test._
import utils.MavenCentral.UnavailableException

// note that each test has its own instance of Global because memcache can't be shared across tests
class MavenCentralSpec extends PlaySpecification {

  "fetchWebJars" should {
    "fail when the searchGroupUrl does not return JSON" in new WithServer(port = testServerPort, app = FakeApplication(additionalConfiguration = Map("webjars.searchGroupUrl" -> s"http://localhost:$testServerPort/asdf"), withGlobal = Some(new GlobalSettings))) {
      await(MavenCentral.fetchWebJars(WebJarCatalog.CLASSIC)) should throwA[UnavailableException]
    }
    "work normally" in new WithApplication(FakeApplication(withGlobal = Some(new GlobalSettings))) {
      await(MavenCentral.fetchWebJars(WebJarCatalog.CLASSIC)).size should beEqualTo (40)
    }
  }

}
