package utils


import akka.util.Timeout
import play.api.test._

import java.net.URL
import scala.concurrent.duration._

class LicenseDetectorSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 120.seconds

  lazy val licenseDetector: LicenseDetector = application.injector.instanceOf[LicenseDetector]

  "licenseDetect" should {
    "convert raw license URL to license" in {
      val result = await(licenseDetector.licenseDetect(new URL("http://polymer.github.io/LICENSE.txt")))
      result.name must be equalTo "BSD 3-Clause"
    }
  }

}