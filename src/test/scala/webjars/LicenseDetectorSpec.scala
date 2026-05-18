package webjars

import webjars.TestInfrastructure.testConfig
import webjars.utils.*
import zio.*
import zio.http.{Client, URL}
import zio.test.*

object LicenseDetectorSpec extends ZIOSpecDefault:

  def spec = suite("LicenseDetector")(
    test("convert raw license URL to license") {
      for
        client <- ZIO.service[Client]
        licenseDetector = LicenseDetectorLive(client, testConfig.githubAuthToken)
        result <- ZIO.scoped(licenseDetector.licenseDetect(URL.unsafeParse("http://polymer.github.io/LICENSE.txt")))
      yield assertTrue(result.name == "BSD 3-Clause")
    } @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(120.seconds)
