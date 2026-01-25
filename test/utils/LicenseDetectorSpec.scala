package utils


import io.lemonlabs.uri.AbsoluteUrl
import org.apache.pekko.util.Timeout
import play.api.test.*

import scala.concurrent.duration.*

class LicenseDetectorSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 120.seconds

  lazy val licenseDetector: LicenseDetector = application.injector.instanceOf[LicenseDetector]

  "licenseDetect" should {
    "convert raw license URL to license" in {
      val result = await(licenseDetector.licenseDetect(AbsoluteUrl.parse("http://polymer.github.io/LICENSE.txt")))
      result.name must be equalTo "BSD 3-Clause"
    }
  }

}
