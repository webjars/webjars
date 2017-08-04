package utils

import org.specs2.mutable._

import scala.util.Random

class SemVerSpec extends Specification {

  private val random = new Random()
  private val alphas = ('a' to 'w') ++ ('A' to 'W') ++ Seq('y', 'z', 'Y', 'Z')

  private val prefix = somePrefix(10)
  private val major1 = somePositiveIntegerBetween(1, 100)
  private val major2 = major1 + somePositiveIntegerBetween(1, 10)
  private val major3 = major2 + somePositiveIntegerBetween(1, 10)
  private val minor1 = somePositiveIntegerBetween(1, 100)
  private val minor2 = minor1 + somePositiveIntegerBetween(1, 10)
  private val patch1 = somePositiveIntegerBetween(1, 100)
  private val suffix = someSuffix(10)
  private val pfx_1 = prefix + major1
  private val pfx_1_2 = s"$pfx_1.$minor1"
  private val pfx_2 = prefix + (major1 + 1)
  private val pfx_1_3 = s"$pfx_1.${minor1 + 1}"
  private val _3_sfx = s"$patch1$suffix"
  private val pfx_1_2_3_sfx = s"$pfx_1.$minor1.${_3_sfx}"
  private val pfx_3 = prefix + major2
  private val pfx_3_2 = s"$pfx_3.$minor1"
  private val pfx_1_4 = s"$pfx_1.$minor2"
  private val pfx_3_2_3_sfx = s"$prefix$major2.$minor1.${_3_sfx}"
  private val pfx_1_4_3_sfx = s"$pfx_1.$minor2.${_3_sfx}"
  private val pfx_4 = prefix + (major2 + 1)
  private val pfx_3_3 = s"$pfx_3.${minor1 + 1}"
  private val pfx_1_5 = s"$pfx_1.${minor2 + 1}"
  private val pfx_0_2_3_sfx = s"${prefix}0.$minor1.${_3_sfx}"
  private val pfx_0_3 = s"${prefix}0.${minor1 + 1}"
  private val pfx_0_0_3_sfx = s"${prefix}0.0.${_3_sfx}"
  private val pfx_0_0_4 = s"${prefix}0.0.${patch1 + 1}"
  private val pfx_5 = prefix + major3
  private val pfx_6 = prefix + (major3 + 1)
  private val a_z_0_9 = someAlphaNumericString(40) + "aBc"

  // SemVer Docs: https://github.com/npm/node-semver

  "SemVerUtil.convertSemVerToMaven" should {
    "work with static versions" in {
      SemVer.convertSemVerToMaven(pfx_1_2_3_sfx)      must be equalTo Some(pfx_1_2_3_sfx)
      SemVer.convertSemVerToMaven(s"=$pfx_1_2_3_sfx") must be equalTo Some(s"$pfx_1_2_3_sfx")
    }
    "work with basic ranges" in {
      SemVer.convertSemVerToMaven(s">$pfx_1")         must be equalTo Some(s"($pfx_1,)")
      SemVer.convertSemVerToMaven(s"> $pfx_1")        must be equalTo Some(s"($pfx_1,)")
      SemVer.convertSemVerToMaven(s">$pfx_1_2")       must be equalTo Some(s"($pfx_1_2,)")
      SemVer.convertSemVerToMaven(s">$pfx_1_2_3_sfx") must be equalTo Some(s"($pfx_1_2_3_sfx,)")

      SemVer.convertSemVerToMaven(s">=$pfx_1")         must be equalTo Some(s"[$pfx_1,)")
      SemVer.convertSemVerToMaven(s">= $pfx_1")        must be equalTo Some(s"[$pfx_1,)")
      SemVer.convertSemVerToMaven(s">=$pfx_1_2")       must be equalTo Some(s"[$pfx_1_2,)")
      SemVer.convertSemVerToMaven(s">=$pfx_1_2_3_sfx") must be equalTo Some(s"[$pfx_1_2_3_sfx,)")
      SemVer.convertSemVerToMaven(s">=$pfx_1.x")       must be equalTo Some(s"[$pfx_1,)")

      SemVer.convertSemVerToMaven(s"<$pfx_1")         must be equalTo Some(s"(,$pfx_1)")
      SemVer.convertSemVerToMaven(s"< $pfx_1")        must be equalTo Some(s"(,$pfx_1)")
      SemVer.convertSemVerToMaven(s"<$pfx_1_2")       must be equalTo Some(s"(,$pfx_1_2)")
      SemVer.convertSemVerToMaven(s"<$pfx_1_2_3_sfx") must be equalTo Some(s"(,$pfx_1_2_3_sfx)")

      SemVer.convertSemVerToMaven(s"<=$pfx_1")         must be equalTo Some(s"(,$pfx_1]")
      SemVer.convertSemVerToMaven(s"<= $pfx_1")        must be equalTo Some(s"(,$pfx_1]")
      SemVer.convertSemVerToMaven(s"<=$pfx_1_2")       must be equalTo Some(s"(,$pfx_1_2]")
      SemVer.convertSemVerToMaven(s"<=$pfx_1_2_3_sfx") must be equalTo Some(s"(,$pfx_1_2_3_sfx]")
    }
    "work with range sets" in {
      SemVer.convertSemVerToMaven(s">=$pfx_1_2_3_sfx <$pfx_1_4_3_sfx")   must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_4_3_sfx)")
      SemVer.convertSemVerToMaven(s">= $pfx_1_2_3_sfx < $pfx_1_4_3_sfx") must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_4_3_sfx)")
      SemVer.convertSemVerToMaven(s">=$pfx_1_2.x <=$pfx_1_4.x")      must be equalTo Some(s"[$pfx_1_2,$pfx_1_4]")
      SemVer.convertSemVerToMaven(s">=$pfx_1_2_3_sfx $pfx_1_4.x")    must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_5)")
      SemVer.convertSemVerToMaven(s">=$pfx_1_2_3_sfx $pfx_1_4_3_sfx")    must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_4_3_sfx]")
    }
    "work with hyphen ranges" in {
      SemVer.convertSemVerToMaven(s"$pfx_1 - $pfx_3")     must be equalTo Some(s"[$pfx_1,$pfx_4)")
      SemVer.convertSemVerToMaven(s"$pfx_1 - $pfx_3_2")   must be equalTo Some(s"[$pfx_1,$pfx_3_3)")
      SemVer.convertSemVerToMaven(s"$pfx_1 - $pfx_3_2_3_sfx") must be equalTo Some(s"[$pfx_1,$pfx_3_2_3_sfx]")

      SemVer.convertSemVerToMaven(s"$pfx_1_2 - $pfx_3")         must be equalTo Some(s"[$pfx_1_2,$pfx_4)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2 - $pfx_1_4")       must be equalTo Some(s"[$pfx_1_2,$pfx_1_5)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2 - $pfx_1_2_3_sfx") must be equalTo Some(s"[$pfx_1_2,$pfx_1_2_3_sfx]")

      SemVer.convertSemVerToMaven(s"$pfx_1_2_3_sfx - $pfx_3")     must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_4)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2_3_sfx - $pfx_1_4")   must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_5)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2_3_sfx - $pfx_1_4_3_sfx") must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_4_3_sfx]")
    }
    "work with X ranges" in {
      SemVer.convertSemVerToMaven("")               must be equalTo Some("[0,)")
      SemVer.convertSemVerToMaven("*")              must be equalTo Some("[0,)")
      SemVer.convertSemVerToMaven(s"$pfx_1")        must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1.x")      must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1.x.x")    must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1.X")      must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1.X.X")    must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1.*")      must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1.*.*")    must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"$pfx_1$suffix") must be equalTo Some(s"[$pfx_1$suffix,$pfx_2)")

      SemVer.convertSemVerToMaven(s"$pfx_1_2")        must be equalTo Some(s"[$pfx_1_2,$pfx_1_3)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2.x")      must be equalTo Some(s"[$pfx_1_2,$pfx_1_3)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2.X")      must be equalTo Some(s"[$pfx_1_2,$pfx_1_3)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2.*")      must be equalTo Some(s"[$pfx_1_2,$pfx_1_3)")
      SemVer.convertSemVerToMaven(s"$pfx_1_2$suffix") must be equalTo Some(s"[$pfx_1_2$suffix,$pfx_1_3)")
    }
    "work with tilde ranges" in {
      SemVer.convertSemVerToMaven(s"~$pfx_1")          must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"~$pfx_1_2")        must be equalTo Some(s"[$pfx_1_2,$pfx_1_3)")
      SemVer.convertSemVerToMaven(s"~$pfx_1_2_3_sfx")  must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_3)")
      SemVer.convertSemVerToMaven(s"~$pfx_1.x")        must be equalTo Some(s"[$pfx_1,$pfx_2)")
      SemVer.convertSemVerToMaven(s"~ $pfx_1_2_3_sfx") must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_1_3)")
    }
    "work with caret ranges" in {
      SemVer.convertSemVerToMaven(s"^$pfx_1_2_3_sfx") must be equalTo Some(s"[$pfx_1_2_3_sfx,$pfx_2)")

      SemVer.convertSemVerToMaven(s"^$pfx_0_2_3_sfx") must be equalTo Some(s"[$pfx_0_2_3_sfx,$pfx_0_3)")
      SemVer.convertSemVerToMaven(s"^$pfx_0_0_3_sfx") must be equalTo Some(s"[$pfx_0_0_3_sfx,$pfx_0_0_4)")

      SemVer.convertSemVerToMaven(s"^$pfx_1_2.x") must be equalTo Some(s"[$pfx_1_2.0,$pfx_2)")
      SemVer.convertSemVerToMaven("^0.0.x")       must be equalTo Some("[0.0.0,0.1)")
      SemVer.convertSemVerToMaven("^0.0")         must be equalTo Some("[0.0.0,0.1)")

      SemVer.convertSemVerToMaven(s"^$pfx_1.x") must be equalTo Some(s"[$pfx_1.0.0,$pfx_2)")
      SemVer.convertSemVerToMaven("^0.x")       must be equalTo Some("[0.0.0,1)")
    }
    "work with crazy stuff" in {
      SemVer.convertSemVerToMaven("latest") must be equalTo Some("[0,)")
      SemVer.convertSemVerToMaven(a_z_0_9)  must be equalTo Some(a_z_0_9)
    }
    "work with || syntax" in {
      SemVer.convertSemVerToMaven(s"^$pfx_1_2.0 || >$pfx_1_2_3_sfx") must be equalTo Some(s"[$pfx_1_2.0,$pfx_2),($pfx_1_2_3_sfx,)")
      SemVer.convertSemVerToMaven(s"$pfx_1 || $pfx_3 || $pfx_5")     must be equalTo Some(s"[$pfx_1,$pfx_2),[$pfx_3,$pfx_4),[$pfx_5,$pfx_6)")
    }
  }

  def somePrefix(length: Int): String = {
    (for (i <- 0 to random.nextInt(length)) yield alphas(random.nextInt(alphas.length))).mkString("")
  }

  def somePositiveIntegerBetween(start: Int, end: Int): Int = {
    start + random.nextInt(end - start)
  }

  def someSuffix(length: Int): String = {
    val chars = alphas ++ Seq('-', '.')
    val randomLength = random.nextInt(length)
    if (randomLength == 0) return ""
    // We clear the first '.' to make sure we never have a string that starts with "-." which is invalid.
    '-' + (for (i <- 0 to randomLength) yield chars(random.nextInt(chars.length))).mkString("").replaceFirst("\\.", "")
  }

  def someAlphaNumericString(length: Int): String = {
    val chars = alphas ++ ('0' to '9')
    (for (i <- 0 to random.nextInt(length)) yield chars(random.nextInt(chars.length))).mkString("")
  }
}
