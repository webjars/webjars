package utils


import play.api.test._

import scala.concurrent.ExecutionContext
import scala.util.Try

class LicenseUtilsSpec extends PlaySpecification {

  val ws = StandaloneWS.apply()
  val licenseUtils = LicenseUtils(ExecutionContext.global, ws.client)

  "gitHubLicenseDetect" should {
    "detect the license" in {
      await(licenseUtils.gitHubLicenseDetect(Try("twbs/bootstrap"))) must beEqualTo("MIT")
    }
    "detect another license" in {
      await(licenseUtils.gitHubLicenseDetect(Try("angular/angular"))) must beEqualTo("Apache-2.0")
    }
  }

  step(ws.close())

}