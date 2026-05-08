package webjars

import webjars.utils.{SemVer, SemVerLive}
import zio.*
import zio.http.Client
import zio.test.*

object SemVerSpec extends ZIOSpecDefault:

  def spec = suite("SemVer")(
    suite("toMaven")(
      test("work") {
        assertTrue(
          SemVer.toMaven("1.2.3").get == "1.2.3",
          SemVer.toMaven(">=1.2.3").get == "[1.2.3,)",
          SemVer.toMaven(">1.2.3").get == "(1.2.3,)",
          SemVer.toMaven("1.2.3||2.1.0").get == "[1.2.3],[2.1.0]",
          SemVer.toMaven(">=1.2.3 <2.0.0-0||2.1.0").get == "[1.2.3,2.0.0-0),[2.1.0]",
          SemVer.toMaven(">=1.2.3 <2.0.0-0||>=2.1.0 <3.0.0-0").get == "[1.2.3,2.0.0-0),[2.1.0,3.0.0-0)",
        )
      },
    ),
    suite("validRange")(
      test("work") {
        for
          client <- ZIO.service[Client]
          semVer = SemVerLive(client)
          r1 <- ZIO.scoped(semVer.validRange("1.2.3"))
          r2 <- ZIO.scoped(semVer.validRange(">1"))
          r3 <- ZIO.scoped(semVer.validRange("1"))
          r4 <- ZIO.scoped(semVer.validRange("^1.2.3"))
          r5 <- ZIO.scoped(semVer.validRange("1.2.3 || 2.1.0"))
          r6 <- ZIO.scoped(semVer.validRange("^1.2.3 || 2.1.0"))
          r7 <- ZIO.scoped(semVer.validRange("^1.2.3 || ^2.1.0"))
        yield assertTrue(
          r1.contains("1.2.3"),
          r2.contains(">=2.0.0"),
          r3.contains(">=1.0.0 <2.0.0-0"),
          r4.contains(">=1.2.3 <2.0.0-0"),
          r5.contains("1.2.3||2.1.0"),
          r6.contains(">=1.2.3 <2.0.0-0||2.1.0"),
          r7.contains(">=1.2.3 <2.0.0-0||>=2.1.0 <3.0.0-0"),
        )
      },
    ) @@ TestAspect.withLiveClock,
    suite("maxSatisfying")(
      test("work") {
        for
          client <- ZIO.service[Client]
          semVer = SemVerLive(client)
          result <- ZIO.scoped(semVer.maxSatisfying(Set("1.0.0", "1.0.1"), ">=1.0.0 <2"))
        yield assertTrue(result.contains("1.0.1"))
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(120.seconds)
