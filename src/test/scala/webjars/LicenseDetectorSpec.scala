package webjars

import io.lemonlabs.uri.AbsoluteUrl
import webjars.TestInfrastructure.testConfig
import webjars.utils.{LicenseDetector, LicenseDetectorLive}
import zio.*
import zio.http.Client
import zio.test.*

object LicenseDetectorSpec extends ZIOSpecDefault:

  def spec = suite("LicenseDetector")(
    test("convert raw license URL to license") {
      for
        client <- ZIO.service[Client]
        licenseDetector = LicenseDetectorLive(client, testConfig.githubAuthToken)
        result <- ZIO.scoped(licenseDetector.licenseDetect(AbsoluteUrl.parse("http://polymer.github.io/LICENSE.txt")))
      yield assertTrue(result.name == "BSD 3-Clause")
    } @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(120.seconds)
