package utils

import scala.language.implicitConversions
import scala.util.Try


object SemVerUtil {

  def convertSemVerToMaven(version: String): Option[String] = {
    val maybeSemVer = parseSemVer(version)

    maybeSemVer.flatMap { semVer =>
      semVer.fold(_.maven, _.maven)
    }
  }

  def parseSemVer(version: String): Option[Either[Version, Range]] = {

    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    object SomeInt {
      def unapply(s: String): Option[Option[Int]] = Some(Try(s.toInt).toOption)
    }

    object SomeString {
      def unapply(s: String): Option[Option[String]] = Some(if (s.isEmpty) { None } else { Some(s) })
    }

    object SomeLimit {
      def unapply(s: String): Option[Limits.Limits] = Some(Limits.withName(s))
    }

    val normalizedXRangeVersion = version.replace("x", "*").replace("X", "*")

    normalizedXRangeVersion match {
      // No version or *
      case "" | "*" =>
        Some(Right(SemVerXRange(SemVerVersionRange(Limits.GTE, Some(0)), SemVerVersionRange(Limits.LT))))
      // 1.2.3 | 1.2.3-alpha | =1.2.3 | =1.2.3-alpha
      case r"^=?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.(\d+)${SomeInt(patch)}-?([\w.]*)${SomeString(tag)}$$" =>
        Some(Left(SemVerVersion(major, minor, patch, tag)))
      // 1 - 2 | 1 - 2.3 | 1 - 2.3.1 | 1 - 2.3.1-alpha
      case r"^(\d+)${SomeInt(leftMajor)}\.?(\d*)${SomeInt(leftMinor)}\.?(\d*)${SomeInt(leftPatch)}-?([\w.]*)${SomeString(leftTag)} - (\d+)${SomeInt(rightMajor)}\.?(\d*)${SomeInt(rightMinor)}\.?(\d*)${SomeInt(rightPatch)}-?([\w.]*)${SomeString(rightTag)}$$" =>
        val leftVersionRange = SemVerVersionRange(Limits.GTE, leftMajor, leftMinor, leftPatch, leftTag)
        val rightVersionRange = SemVerVersionRange(Limits.LTE, rightMajor, rightMinor, rightPatch, rightTag)
        Some(Right(SemVerHyphenRange(leftVersionRange, rightVersionRange)))
      // 1.1.* | 1.2
      case r"^(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.?\*?-?([\w.]*)${SomeString(tag)}$$" =>
        val leftVersionRange = SemVerVersionRange(limit = Limits.GTE, major = major, minor = minor, tag = tag)
        val rightVersionRange = SemVerVersionRange(limit = Limits.LT, major = major, minor = minor.map(_ + 1), tag = tag)
        Some(Right(SemVerXRange(leftVersionRange, rightVersionRange)))
      // 1.* | 1
      case r"^(\d+)${SomeInt(major)}\.?\*?-?([\w.]*)${SomeString(tag)}$$" =>
        val leftVersionRange = SemVerVersionRange(limit = Limits.GTE, major = major, tag = tag)
        val rightVersionRange = SemVerVersionRange(limit = Limits.LT, major = major.map(_ + 1), tag = tag)
        Some(Right(SemVerXRange(leftVersionRange, rightVersionRange)))
      // >=1.2.3 | <=1.2.3 | >1.2.3 | <1.2.3
      case r"^(>=|<=|>|<)${SomeLimit(limit)} ?(\d+)${SomeInt(major)}\.?(\d*)${SomeInt(minor)}\.?(\d*)${SomeInt(patch)}-?([\w.]*)${SomeString(tag)}$$" =>
        Some(Left(SemVerVersionRange(limit, major, minor, patch, tag)))
      // >=1.0.0 <1.4.0
      case r"^(>=|<=|>|<) ?${SomeLimit(leftLimit)}(\d+)${SomeInt(leftMajor)}\.?(\d*|\*)${SomeInt(leftMinor)}\.?(\d*|\*)${SomeInt(leftPatch)}-?([\w.]*)${SomeString(leftTag)} (>=|<=|>|<) ?${SomeLimit(rightLimit)}(\d+)${SomeInt(rightMajor)}\.?(\d*|\*)${SomeInt(rightMinor)}\.?(\d*|\*)${SomeInt(rightPatch)}-?([\w.]*)${SomeString(rightTag)}$$" =>
        val leftVersionRange = SemVerVersionRange(leftLimit, leftMajor, leftMinor, leftPatch, leftTag)
        val rightVersionRange = SemVerVersionRange(rightLimit, rightMajor, rightMinor, rightPatch, rightTag)
        Some(Right(SemVerSetRange(leftVersionRange, rightVersionRange)))
      // ~1
      case r"^~(\d+)${SomeInt(major)}$$" =>
        val leftVersionRange = SemVerVersionRange(limit = Limits.GTE, major = major)
        val rightVersionRange = SemVerVersionRange(limit = Limits.LT, major = major.map(_ + 1))
        Some(Right(SemVerTildeRange(leftVersionRange, rightVersionRange)))
      // ~1.2 | ~1.2.3
      case r"^~(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.?(\d*)${SomeInt(patch)}-?([\w.]*)${SomeString(tag)}$$" =>
        val leftVersionRange = SemVerVersionRange(limit = Limits.GTE, major = major, minor = minor, patch = patch, tag = tag)
        val rightVersionRange = SemVerVersionRange(limit = Limits.LT, major = major, minor = minor.map(_ + 1))
        Some(Right(SemVerTildeRange(leftVersionRange, rightVersionRange)))
      // ^1.2.3 | ^0.0 | ^0 | ^1.2.x | ^1.x | ^0.0.x | ^0.x | ~1.x
      case r"^[\^~](\d+)${SomeInt(major)}\.?(\d*|\*)${SomeInt(minor)}\.?(\d*|\*)${SomeInt(patch)}-?([\w.]*)${SomeString(tag)}$$" =>
        val desugaredMinor = minor.orElse(Some(0))
        val desugaredPatch = patch.orElse(Some(0))

        val leftVersionRange = SemVerVersionRange(Limits.GTE, major, desugaredMinor, desugaredPatch, tag)
        val rightVersionRange = if (major == Some(0) && minor == Some(0) && patch.isDefined) {
          SemVerVersionRange(Limits.LT, major, minor, patch.map(_ + 1), None)
        }
        else if (major == Some(0) && minor.isDefined) {
          SemVerVersionRange(Limits.LT, major, minor.map(_ + 1), None, None)
        }
        else {
          SemVerVersionRange(Limits.LT, major.map(_ + 1), None, None, None)
        }
        Some(Right(SemVerCaretRange(leftVersionRange, rightVersionRange)))
      case _ =>
        None
    }
  }

}

object Limits extends Enumeration {
  type Limits = Value
  val LT = Value("<")
  val LTE = Value("<=")
  val GT = Value(">")
  val GTE = Value(">=")
}

trait Version {
  val major: Option[Int]
  val minor: Option[Int]
  val patch: Option[Int]
  val tag: Option[String]
  override def toString: String = {
    Seq(major, minor, patch).filter(_.isDefined).map(_.get).mkString(".") + tag.fold("")("-" + _)
  }
  def maven: Option[String] = {
    Some(toString)
  }
}

trait VersionRange extends Version {
  val limit: Limits.Limits
  override def maven: Option[String] = {
    limit match {
      case Limits.LT => Some("(," + super.toString + ")")
      case Limits.LTE => Some("(," + super.toString + "]")
      case Limits.GT => Some("(" + super.toString + ",)")
      case Limits.GTE => Some("[" + super.toString + ",)")
      case _ => None
    }
  }
}

trait Range {
  val left: VersionRange
  val right: VersionRange
  def maven: Option[String] = {
    (left.limit, right.limit) match {
      case (Limits.GT, Limits.LT) => Some("(" + left.toString + "," + right.toString + ")")
      case (Limits.GT, Limits.LTE) => Some("(" + left.toString + "," + right.toString + "]")
      case (Limits.GTE, Limits.LT) => Some("[" + left.toString + "," + right.toString + ")")
      case (Limits.GTE, Limits.LTE) => Some("[" + left.toString + "," + right.toString + "]")
      case _ => None
    }
  }
}

case class SemVerVersion(
  major: Option[Int] = None,
  minor: Option[Int] = None,
  patch: Option[Int] = None,
  tag: Option[String] = None) extends Version

case class SemVerVersionRange(
  limit: Limits.Limits,
  major: Option[Int] = None,
  minor: Option[Int] = None,
  patch: Option[Int] = None,
  tag: Option[String] = None) extends VersionRange

case class SemVerHyphenRange(left: VersionRange, right: VersionRange) extends Range

case class SemVerXRange(left: VersionRange, right: VersionRange) extends Range

case class SemVerTildeRange(left: VersionRange, right: VersionRange) extends Range

case class SemVerCaretRange(left: VersionRange, right: VersionRange) extends Range

case class SemVerSetRange(left: VersionRange, right: VersionRange) extends Range