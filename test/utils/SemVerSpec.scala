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
      SemVer.toMaven(">=1.2.3 <2.0.0-0||2.1.0") must beASuccessfulTry("[1.2.3,2.0.0),[2.1.0]")
      SemVer.toMaven(">=1.2.3 <2.0.0-0||>=2.1.0 <3.0.0-0") must beASuccessfulTry("[1.2.3,2.0.0),[2.1.0,3.0.0)")
    }
    "strip -0 suffix to exclude pre-releases" in {
      SemVer.toMaven(">=1.7.18 <2.0.0-0") must beASuccessfulTry("[1.7.18,2.0.0)")
      SemVer.toMaven("<2.0.0-0") must beASuccessfulTry("(,2.0.0)")
      SemVer.toMaven("<=2.0.0-0") must beASuccessfulTry("(,2.0.0]")
    }
  }

}
