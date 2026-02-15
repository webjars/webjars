package utils

import play.api.test.PlaySpecification

class SemVerSpec extends PlaySpecification with GlobalApplication {

  lazy val semVer = application.injector.instanceOf[SemVer]

  "validRange" should {
    "work" in {
      await(semVer.validRange("1.2.3")) must beSome("1.2.3")
      await(semVer.validRange(">1")) must beSome(">=2.0.0")
      await(semVer.validRange("1")) must beSome(">=1.0.0 <2.0.0-0")
      await(semVer.validRange("^1.2.3")) must beSome(">=1.2.3 <2.0.0-0")
      await(semVer.validRange("1.2.3 || 2.1.0")) must beSome("1.2.3||2.1.0")
      await(semVer.validRange("^1.2.3 || 2.1.0")) must beSome(">=1.2.3 <2.0.0-0||2.1.0")
      await(semVer.validRange("^1.2.3 || ^2.1.0")) must beSome(">=1.2.3 <2.0.0-0||>=2.1.0 <3.0.0-0")
    }
  }

  "maxSatisfying" should {
    "work" in {
      await(semVer.maxSatisfying(Set("1.0.0", "1.0.1"), ">=1.0.0 <2")) must beSome("1.0.1")
    }
  }

  "toMaven" should {
    "work" in {
      SemVer.toMaven("1.2.3") must beASuccessfulTry("1.2.3")
      SemVer.toMaven(">=1.2.3") must beASuccessfulTry("[1.2.3,)")
      SemVer.toMaven(">1.2.3") must beASuccessfulTry("(1.2.3,)")
      SemVer.toMaven("1.2.3||2.1.0") must beASuccessfulTry("[1.2.3],[2.1.0]")
      SemVer.toMaven(">=1.2.3 <2.0.0-0||2.1.0") must beASuccessfulTry("[1.2.3,1.999999.999999],[2.1.0]")
      SemVer.toMaven(">=1.2.3 <2.0.0-0||>=2.1.0 <3.0.0-0") must beASuccessfulTry("[1.2.3,1.999999.999999],[2.1.0,2.999999.999999]")
    }
    "convert NPM -0 suffix to high version number to exclude pre-releases" in {
      SemVer.toMaven(">=1.7.18 <2.0.0-0") must beASuccessfulTry("[1.7.18,1.999999.999999]")
      SemVer.toMaven(">=3.3.11 <4.0.0-0") must beASuccessfulTry("[3.3.11,3.999999.999999]")
      SemVer.toMaven(">=1.2.3 <1.3.0-0") must beASuccessfulTry("[1.2.3,1.2.999999]")
      SemVer.toMaven(">=0.2.3 <0.3.0-0") must beASuccessfulTry("[0.2.3,0.2.999999]")
      SemVer.toMaven(">=0.0.3 <0.0.4-0") must beASuccessfulTry("[0.0.3,0.0.3]")
      SemVer.toMaven("<2.0.0-0") must beASuccessfulTry("(,1.999999.999999]")
      SemVer.toMaven("<=2.0.0-0") must beASuccessfulTry("(,1.999999.999999]")
    }
    "handle edge case of <0.0.0-0 that would result in negative version" in {
      // When conversion fails (would result in negative version), the input is returned unchanged.
      // This is handled by the fallback in toMaven() which wraps unconverted single-element ranges.
      SemVer.toMaven("<0.0.0-0") must beASuccessfulTry("<0.0.0-0")
      SemVer.toMaven("<=0.0.0-0") must beASuccessfulTry("<=0.0.0-0")
    }
  }

}
