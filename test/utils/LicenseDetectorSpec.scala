package utils


import java.net.URL

import akka.util.Timeout
import play.api.test._

import scala.concurrent.duration._

class LicenseDetectorSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 120.seconds

  lazy val licenseDetector: LicenseDetector = application.injector.instanceOf[LicenseDetector]

  "gitHubLicenseDetect" should {
    "detect the license" in {
      await(licenseDetector.gitHubLicenseDetect(Some("twbs/bootstrap"))) must beEqualTo("MIT")
    }
    "detect another license" in {
      await(licenseDetector.gitHubLicenseDetect(Some("angular/angular"))) must beEqualTo("MIT")
    }
    "detect another license" in {
      await(licenseDetector.gitHubLicenseDetect(Some("T00rk/bootstrap-material-datetimepicker"))) must beEqualTo("MIT")
    }
  }

  "validLicenses" should {
    "convert licenses to accepted ones" in {
      val licenses = Seq("BSD 2-Clause", "BSD-2-Clause", "bsd2clause", "GPLv2", "GPLv3", "MIT/X11")
      val result = licenseDetector.validLicenses(licenses)
      result must containAllOf(Seq("GPL-2.0", "BSD 2-Clause", "GPL-3.0", "MIT"))
    }
    "convert SPDX to BinTray" in {
      val licenses = Seq("OFL-1.1")
      val result = licenseDetector.validLicenses(licenses)
      result must be equalTo Seq("Openfont-1.1")
    }
    "fail to convert incompatible licenses" in {
      licenseDetector.validLicenses(Seq("foo")) must beEmpty
    }
    "succeed with at least one valid license" in {
      licenseDetector.validLicenses(Seq("foo", "MIT")) must be equalTo Seq("MIT")
    }
    "convert New BSD License to BSD 3-Clause" in {
      licenseDetector.validLicenses(Seq("New BSD License")) must be equalTo Seq("BSD 3-Clause")
    }
    "use a case insensitive match" in {
      licenseDetector.validLicenses(Seq("UNLICENSE")) must be equalTo Seq("Unlicense")
    }
  }

  "licenseDetect" should {
    "convert raw license URL to license" in {
      val result = await(licenseDetector.licenseDetect(new URL("http://polymer.github.io/LICENSE.txt")))
      result must be equalTo "BSD 3-Clause"
    }
  }

}
