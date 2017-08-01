package utils

import org.specs2.mutable._

class SemVerSpec extends Specification {

  // SemVer Docs: https://github.com/npm/node-semver

  "SemVerUtil.convertSemVerToMaven" should {
    "work with static versions" in {
      SemVer.convertSemVerToMaven("1.2.3")       must be equalTo Some("1.2.3")
      SemVer.convertSemVerToMaven("=1.2.3")      must be equalTo Some("1.2.3")
      SemVer.convertSemVerToMaven("1.2.3-alpha") must be equalTo Some("1.2.3-alpha")
      SemVer.convertSemVerToMaven("1.2.3-dev-r") must be equalTo Some("1.2.3-dev-r")
    }
    "work with basic ranges" in {
      SemVer.convertSemVerToMaven(">1")           must be equalTo Some("(1,)")
      SemVer.convertSemVerToMaven("> 1")          must be equalTo Some("(1,)")
      SemVer.convertSemVerToMaven(">1.2")         must be equalTo Some("(1.2,)")
      SemVer.convertSemVerToMaven(">1.2.3")       must be equalTo Some("(1.2.3,)")
      SemVer.convertSemVerToMaven(">1.2.3-alpha") must be equalTo Some("(1.2.3-alpha,)")
      SemVer.convertSemVerToMaven(">1.2.3-dev-r") must be equalTo Some("(1.2.3-dev-r,)")

      SemVer.convertSemVerToMaven(">=1")           must be equalTo Some("[1,)")
      SemVer.convertSemVerToMaven(">= 1")          must be equalTo Some("[1,)")
      SemVer.convertSemVerToMaven(">=1.2")         must be equalTo Some("[1.2,)")
      SemVer.convertSemVerToMaven(">=1.2.3")       must be equalTo Some("[1.2.3,)")
      SemVer.convertSemVerToMaven(">=1.2.3-alpha") must be equalTo Some("[1.2.3-alpha,)")
      SemVer.convertSemVerToMaven(">=1.2.3-dev-r") must be equalTo Some("[1.2.3-dev-r,)")
      SemVer.convertSemVerToMaven(">=1.9.x")       must be equalTo Some("[1.9,)")

      SemVer.convertSemVerToMaven("<1")           must be equalTo Some("(,1)")
      SemVer.convertSemVerToMaven("< 1")          must be equalTo Some("(,1)")
      SemVer.convertSemVerToMaven("<1.2")         must be equalTo Some("(,1.2)")
      SemVer.convertSemVerToMaven("<1.2.3")       must be equalTo Some("(,1.2.3)")
      SemVer.convertSemVerToMaven("<1.2.3-alpha") must be equalTo Some("(,1.2.3-alpha)")
      SemVer.convertSemVerToMaven("<1.2.3-dev-r") must be equalTo Some("(,1.2.3-dev-r)")

      SemVer.convertSemVerToMaven("<=1")           must be equalTo Some("(,1]")
      SemVer.convertSemVerToMaven("<= 1")          must be equalTo Some("(,1]")
      SemVer.convertSemVerToMaven("<=1.2")         must be equalTo Some("(,1.2]")
      SemVer.convertSemVerToMaven("<=1.2.3")       must be equalTo Some("(,1.2.3]")
      SemVer.convertSemVerToMaven("<=1.2.3-alpha") must be equalTo Some("(,1.2.3-alpha]")
      SemVer.convertSemVerToMaven("<=1.2.3-dev-r") must be equalTo Some("(,1.2.3-dev-r]")
    }
    "work with range sets" in {
      SemVer.convertSemVerToMaven(">=1.0.0 <1.4.0")      must be equalTo Some("[1.0.0,1.4.0)")
      SemVer.convertSemVerToMaven(">= 0.10.1 < 0.12.0")  must be equalTo Some("[0.10.1,0.12.0)")
      SemVer.convertSemVerToMaven(">=1.2.x <=1.4.x")     must be equalTo Some("[1.2,1.4]")
      SemVer.convertSemVerToMaven(">=1.2.16 1.4.x")      must be equalTo Some("[1.2.16,1.5)")
      SemVer.convertSemVerToMaven(">=1.2.16 1.4.0")      must be equalTo Some("[1.2.16,1.4.0]")
    }
    "work with hyphen ranges" in {
      SemVer.convertSemVerToMaven("1 - 2")           must be equalTo Some("[1,3)")
      SemVer.convertSemVerToMaven("1 - 2.4")         must be equalTo Some("[1,2.5)")
      SemVer.convertSemVerToMaven("1 - 2.3.4")       must be equalTo Some("[1,2.3.4]")
      SemVer.convertSemVerToMaven("1 - 2.3.4-alpha") must be equalTo Some("[1,2.3.4-alpha]")
      SemVer.convertSemVerToMaven("1 - 2.3.4-dev-r") must be equalTo Some("[1,2.3.4-dev-r]")

      SemVer.convertSemVerToMaven("1.2 - 2")           must be equalTo Some("[1.2,3)")
      SemVer.convertSemVerToMaven("1.2 - 1.4")         must be equalTo Some("[1.2,1.5)")
      SemVer.convertSemVerToMaven("1.2 - 1.2.3")       must be equalTo Some("[1.2,1.2.3]")
      SemVer.convertSemVerToMaven("1.2 - 1.2.3-alpha") must be equalTo Some("[1.2,1.2.3-alpha]")
      SemVer.convertSemVerToMaven("1.2 - 1.2.3-dev-r") must be equalTo Some("[1.2,1.2.3-dev-r]")

      SemVer.convertSemVerToMaven("1.2.3 - 2")           must be equalTo Some("[1.2.3,3)")
      SemVer.convertSemVerToMaven("1.2.3 - 1.4")         must be equalTo Some("[1.2.3,1.5)")
      SemVer.convertSemVerToMaven("1.2.3 - 1.3.4")       must be equalTo Some("[1.2.3,1.3.4]")
      SemVer.convertSemVerToMaven("1.2.3 - 1.3.4-alpha") must be equalTo Some("[1.2.3,1.3.4-alpha]")
      SemVer.convertSemVerToMaven("1.2.3 - 1.3.4-dev-r") must be equalTo Some("[1.2.3,1.3.4-dev-r]")

      SemVer.convertSemVerToMaven("1.2.3-alpha - 2")           must be equalTo Some("[1.2.3-alpha,3)")
      SemVer.convertSemVerToMaven("1.2.3-dev-r - 2")           must be equalTo Some("[1.2.3-dev-r,3)")
      SemVer.convertSemVerToMaven("1.2.3-alpha - 1.4")         must be equalTo Some("[1.2.3-alpha,1.5)")
      SemVer.convertSemVerToMaven("1.2.3-dev-r - 1.4")         must be equalTo Some("[1.2.3-dev-r,1.5)")
      SemVer.convertSemVerToMaven("1.2.3-alpha - 1.3.4")       must be equalTo Some("[1.2.3-alpha,1.3.4]")
      SemVer.convertSemVerToMaven("1.2.3-dev-r - 1.3.4")       must be equalTo Some("[1.2.3-dev-r,1.3.4]")
      SemVer.convertSemVerToMaven("1.2.3-alpha - 1.3.4-alpha") must be equalTo Some("[1.2.3-alpha,1.3.4-alpha]")
      SemVer.convertSemVerToMaven("1.2.3-dev-r - 1.3.4-dev-r") must be equalTo Some("[1.2.3-dev-r,1.3.4-dev-r]")
    }
    "work with X ranges" in {
      SemVer.convertSemVerToMaven("")      must be equalTo Some("[0,)")
      SemVer.convertSemVerToMaven("*")     must be equalTo Some("[0,)")
      SemVer.convertSemVerToMaven("1")     must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1.x")   must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1.x.x") must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1.X")   must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1.X.X") must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1.*")   must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1.*.*") must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("1-alpha") must be equalTo Some("[1-alpha,2)")

      SemVer.convertSemVerToMaven("1.2")       must be equalTo Some("[1.2,1.3)")
      SemVer.convertSemVerToMaven("1.2.x")     must be equalTo Some("[1.2,1.3)")
      SemVer.convertSemVerToMaven("1.2.X")     must be equalTo Some("[1.2,1.3)")
      SemVer.convertSemVerToMaven("1.2.*")     must be equalTo Some("[1.2,1.3)")
      SemVer.convertSemVerToMaven("1.2-alpha") must be equalTo Some("[1.2-alpha,1.3)")
    }
    "work with tilde ranges" in {
      SemVer.convertSemVerToMaven("~1")           must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("~1.2")         must be equalTo Some("[1.2,1.3)")
      SemVer.convertSemVerToMaven("~1.2.3")       must be equalTo Some("[1.2.3,1.3)")
      SemVer.convertSemVerToMaven("~1.2.3-alpha") must be equalTo Some("[1.2.3-alpha,1.3)")
      SemVer.convertSemVerToMaven("~1.2.3-dev-r") must be equalTo Some("[1.2.3-dev-r,1.3)")
      SemVer.convertSemVerToMaven("~1.x")         must be equalTo Some("[1,2)")
      SemVer.convertSemVerToMaven("~ 0.1.11")     must be equalTo Some("[0.1.11,0.2)")
    }
    "work with caret ranges" in {
      SemVer.convertSemVerToMaven("^1.2.3")        must be equalTo Some("[1.2.3,2)")
      SemVer.convertSemVerToMaven("^1.2.3-beta.2") must be equalTo Some("[1.2.3-beta.2,2)")

      SemVer.convertSemVerToMaven("^0.2.3")        must be equalTo Some("[0.2.3,0.3)")
      SemVer.convertSemVerToMaven("^0.0.3")        must be equalTo Some("[0.0.3,0.0.4)")
      SemVer.convertSemVerToMaven("^0.0.3-beta")   must be equalTo Some("[0.0.3-beta,0.0.4)")

      SemVer.convertSemVerToMaven("^1.2.x") must be equalTo Some("[1.2.0,2)")
      SemVer.convertSemVerToMaven("^0.0.x") must be equalTo Some("[0.0.0,0.1)")
      SemVer.convertSemVerToMaven("^0.0")   must be equalTo Some("[0.0.0,0.1)")

      SemVer.convertSemVerToMaven("^1.x") must be equalTo Some("[1.0.0,2)")
      SemVer.convertSemVerToMaven("^0.x") must be equalTo Some("[0.0.0,1)")
    }
    "work with crazy stuff" in {
      SemVer.convertSemVerToMaven("latest") must be equalTo Some("[0,)")
      SemVer.convertSemVerToMaven("b4e74e38e43ac53af8acd62c78c9213be0194245") must be equalTo Some("b4e74e38e43ac53af8acd62c78c9213be0194245")
      SemVer.convertSemVerToMaven("2432d39a1693ccd728cbe7eb55810063737d3403") must be equalTo Some("2432d39a1693ccd728cbe7eb55810063737d3403")
    }
    "work with || syntax" in {
      SemVer.convertSemVerToMaven("^1.3.0 || >1.4.0-beta.0")  must be equalTo Some("[1.3.0,2),(1.4.0-beta.0,)")
      SemVer.convertSemVerToMaven("2 || 3 || 4")              must be equalTo Some("[2,3),[3,4),[4,5)")
    }
    "work with alpha prefix" in {
      SemVer.convertSemVerToMaven("a1.2.3") must be equalTo Some("a1.2.3")

      SemVer.convertSemVerToMaven(">bc1")  must be equalTo Some("(bc1,)")
      SemVer.convertSemVerToMaven("<def1") must be equalTo Some("(,def1)")

      SemVer.convertSemVerToMaven(">=g1")             must be equalTo Some("[g1,)")
      SemVer.convertSemVerToMaven("<=h1")             must be equalTo Some("(,h1]")
      SemVer.convertSemVerToMaven(">=i1.0.0 <i1.4.0") must be equalTo Some("[i1.0.0,i1.4.0)")

      SemVer.convertSemVerToMaven("j1 - j2")           must be equalTo Some("[j1,j3)")
      SemVer.convertSemVerToMaven("k1.2 - k2")         must be equalTo Some("[k1.2,k3)")
      SemVer.convertSemVerToMaven("l1.2.3 - l2")       must be equalTo Some("[l1.2.3,l3)")
      SemVer.convertSemVerToMaven("m1.2.3-alpha - m2") must be equalTo Some("[m1.2.3-alpha,m3)")

      SemVer.convertSemVerToMaven("n1.2") must be equalTo Some("[n1.2,n1.3)")

      SemVer.convertSemVerToMaven("~o1") must be equalTo Some("[o1,o2)")

      SemVer.convertSemVerToMaven("^p1.2.3") must be equalTo Some("[p1.2.3,p2)")
      SemVer.convertSemVerToMaven("^q0.2.3") must be equalTo Some("[q0.2.3,q0.3)")
      SemVer.convertSemVerToMaven("^r1.2.x") must be equalTo Some("[r1.2.0,r2)")
      SemVer.convertSemVerToMaven("^s1.x")   must be equalTo Some("[s1.0.0,s2)")

      SemVer.convertSemVerToMaven("^t1.3.0 || >t1.4.0-beta.0")  must be equalTo Some("[t1.3.0,t2),(t1.4.0-beta.0,)")
    }
  }

}
