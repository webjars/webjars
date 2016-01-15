package utils


import akka.util.Timeout
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

class LicenseUtilsSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  val ws = StandaloneWS.apply()
  val git = GitUtil(ExecutionContext.global, ws.client)
  val licenseUtils = LicenseUtils(ExecutionContext.global, ws.client, git)

  "gitHubLicenseDetect" should {
    "detect the license" in {
      await(licenseUtils.gitHubLicenseDetect(Try("twbs/bootstrap"))) must beEqualTo("MIT")
    }
    "detect another license" in {
      await(licenseUtils.gitHubLicenseDetect(Try("angular/angular"))) must beEqualTo("MIT")
    }
    "detect another license" in {
      await(licenseUtils.gitHubLicenseDetect(Try("T00rk/bootstrap-material-datetimepicker"))) must beEqualTo("MIT")
    }
  }

  step(ws.close())

}