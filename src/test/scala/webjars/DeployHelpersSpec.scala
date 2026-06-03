package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import zio.test.*

object DeployHelpersSpec extends ZIOSpecDefault:

  def spec = suite("Deploy")(
    suite("parseGav")(
      test("parses a normal GAV") {
        assertTrue(
          Deploy.parseGav("org.webjars:jquery-ui:1.14.2") ==
            Some(MavenCentral.gav("org.webjars", "jquery-ui", "1.14.2"))
        )
      },
      test("parses a GAV with a build-metadata version") {
        assertTrue(
          Deploy.parseGav("org.webjars:jquery-ui:1.14.2+1") ==
            Some(MavenCentral.gav("org.webjars", "jquery-ui", "1.14.2+1"))
        )
      },
      test("rejects malformed input") {
        assertTrue(
          Deploy.parseGav("").isEmpty,
          Deploy.parseGav("org.webjars").isEmpty,
          Deploy.parseGav("org.webjars:jquery-ui").isEmpty,
          Deploy.parseGav("org.webjars:jquery-ui:").isEmpty,
          Deploy.parseGav(":jquery-ui:1.14.2").isEmpty,
          Deploy.parseGav("a:b:c:d").isEmpty,
        )
      },
    ),
    suite("nextReleaseVersion")(
      test("returns the upstream version unchanged when it's not yet published") {
        assertTrue(Deploy.nextReleaseVersion("1.14.2", Set.empty) == "1.14.2")
        assertTrue(Deploy.nextReleaseVersion("1.14.2", Set("1.14.0", "1.14.1")) == "1.14.2")
      },
      test("strips a leading v from the upstream version") {
        assertTrue(Deploy.nextReleaseVersion("v1.14.2", Set.empty) == "1.14.2")
      },
      test("returns +1 when the upstream version is already published") {
        assertTrue(Deploy.nextReleaseVersion("1.14.2", Set("1.14.2")) == "1.14.2+1")
      },
      test("picks max+1 when there are existing +N redeploys") {
        assertTrue(
          Deploy.nextReleaseVersion("1.14.2", Set("1.14.2", "1.14.2+1", "1.14.2+2")) == "1.14.2+3"
        )
      },
      test("ignores +N entries that don't parse as a positive integer") {
        assertTrue(
          Deploy.nextReleaseVersion("1.14.2", Set("1.14.2", "1.14.2+abc", "1.14.2+0", "1.14.2+2")) == "1.14.2+3"
        )
      },
      test("doesn't mistake a different base version's +N for ours") {
        assertTrue(
          Deploy.nextReleaseVersion("1.14.2", Set("1.14.2", "1.14.20+5", "1.14.2.1+9")) == "1.14.2+1"
        )
      },
    ),
  )
