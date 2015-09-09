package utils

import scala.language.implicitConversions
import scala.util.Try


object SemVerUtil {

  object Operator extends Enumeration {
    type Operator = Value
    val LT = Value("<")
    val LTE = Value("<=")
    val GT = Value(">")
    val GTE = Value(">=")
  }

  case class Version(
                      major: Option[Int] = None,
                      minor: Option[Int] = None,
                      patch: Option[Int] = None,
                      tag: Option[String] = None) {

    override def toString: String = {
      Seq(major, minor, patch).filter(_.isDefined).map(_.get).mkString(".") + tag.fold("")("-" + _)
    }

    def maven: Option[String] = {
      Some(toString)
    }

  }

  case class Comparator(operator: Operator.Operator, version: Version)

  type VersionRange = Seq[Comparator]
  def VersionRange(comparators: Comparator*) = Seq(comparators: _*)

  object SomeInt {
    def unapply(s: String): Option[Option[Int]] = Some(Try(s.toInt).toOption)
  }

  object SomeString {
    def unapply(s: String): Option[Option[String]] = Some(if (s.isEmpty) { None } else { Some(s) })
  }

  object SomeOperator {
    def unapply(s: String): Option[Operator.Operator] = Some(Operator.withName(s))
  }

  object MaybeOperator {
    def unapply(s: String): Option[Option[Operator.Operator]] = Some(Try(Operator.withName(s)).toOption)
  }

  def versionRangeToMaven(versionRange: VersionRange): Option[String] = {

    def leftBound(comparator: Comparator): Option[String] = comparator.operator match {
      case Operator.GT | Operator.LT => Some("(")
      case Operator.GTE | Operator.LTE => Some("[")
      case _ => None
    }

    def rightBound(comparator: Comparator): Option[String] = comparator.operator match {
      case Operator.GT | Operator.LT => Some(")")
      case Operator.GTE | Operator.LTE => Some("]")
      case _ => None
    }

    if (versionRange.isEmpty) {
      None
    }
    else {
      // todo: not a true intersection

      val validVersionRange: VersionRange = if (versionRange.size == 1) {
        // deal with single ranges where one side is infinity
        if ((versionRange.head.operator == Operator.LT) || (versionRange.head.operator == Operator.LTE)) {
           Comparator(Operator.LT, Version()) +: versionRange
        }
        else {
          versionRange :+ Comparator(Operator.GT, Version())
        }
      }
      else {
        versionRange
      }

      for {
        leftBound <- leftBound(validVersionRange.head)
        leftVersion <- validVersionRange.head.version.maven
        rightBound <- rightBound(validVersionRange.last)
        rightVersion <- validVersionRange.last.version.maven
      } yield {
        leftBound + leftVersion + "," + rightVersion + rightBound
      }

    }
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def convertSemVerToMaven(versionString: String): Option[String] = {

    val normalizedXRangeVersion = versionString.replace("x", "*").replace("X", "*")

    val normalizedComparatorVersion = normalizedXRangeVersion.replaceAllLiterally("> ", ">").replaceAllLiterally("< ", "<").replaceAllLiterally("= ", "=")

    val normalizedOrOrVersion = normalizedComparatorVersion.replaceAllLiterally(" || ", "||")

    object OrOr {
      def unapply(s: String): Option[Seq[String]] = {
        if (s.contains("||")) {
          Some(s.split("\\|\\|"))
        }
        else {
          None
        }
      }
    }

    normalizedOrOrVersion match {
      case OrOr(versionStrings) =>
        val versions = versionStrings.flatMap(parseSemVer)
        if (versions.isEmpty) {
          None
        }
        else {
          Some(versions.flatMap(_.fold(_.maven, versionRangeToMaven)).mkString(","))
        }
      case r"^(>=|<=|>|<)${SomeOperator(leftOperator)}([^\s>=<]+)$leftVersion\s(>=|<=|>|<)?${MaybeOperator(maybeRightOperator)}([^\s>=<]+)$rightVersion$$" =>
        val maybeLeftVersion = parseSemVer(leftOperator + leftVersion)
        val maybeRightVersion = parseSemVer(maybeRightOperator.getOrElse("") + rightVersion)

        for {
          leftVersion <- maybeLeftVersion
          rightVersion <- maybeRightVersion
          leftComparator = leftVersion.fold(Comparator(Operator.GTE, _), _.head)
          rightComparator = rightVersion.fold(Comparator(Operator.LTE, _), _.last)
          semVer <- versionRangeToMaven(VersionRange(leftComparator, rightComparator))
        } yield semVer
      case _ =>
        parseSemVer(normalizedOrOrVersion).flatMap(_.fold(_.maven, versionRangeToMaven))
    }
  }

  def parseSemVer(version: String): Option[Either[Version, VersionRange]] = {

    version match {
      // No version or *
      case "" | "*" =>
        Some(Right(VersionRange(Comparator(Operator.GTE, Version(Some(0))))))
      // 1.2.3 | 1.2.3-alpha | =1.2.3 | =1.2.3-alpha
      case r"^=?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.(\d+)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        Some(Left(Version(major, minor, patch, tag)))
      // 1 - 2 | 1 - 2.3 | 1 - 2.3.1 | 1 - 2.3.1-alpha
      case r"^(\d+)${SomeInt(leftMajor)}\.?(\d*)${SomeInt(leftMinor)}\.?(\d*)${SomeInt(leftPatch)}-?([\w.-]*)${SomeString(leftTag)} - (\d+)${SomeInt(rightMajor)}\.?(\d*)${SomeInt(rightMinor)}\.?(\d*)${SomeInt(rightPatch)}-?([\w.-]*)${SomeString(rightTag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(leftMajor, leftMinor, leftPatch, leftTag))
        val rightVersionRange = Comparator(Operator.LTE, Version(rightMajor, rightMinor, rightPatch, rightTag))
        Some(Right(VersionRange(leftVersionRange, rightVersionRange)))
      // 1.1.* | 1.2
      case r"^(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.?\*?-?([\w.-]*)${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, minor, None, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major, minor.map(_ + 1), None, tag))
        Some(Right(VersionRange(leftVersionRange, rightVersionRange)))
      // 1.* | 1.*.* | 1
      case r"^(\d+)${SomeInt(major)}(?:\.\*){0,3}?-?([\w.-]*)${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, None, None, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major.map(_ + 1), None, None, tag))
        Some(Right(VersionRange(leftVersionRange, rightVersionRange)))
      // >=1.2.3 | <=1.2.3 | >1.2.3 | <1.2.3 | >=1.0.x
      case r"^(>=|<=|>|<)${SomeOperator(operator)} ?(\d+)${SomeInt(major)}\.?(\d*|\*)${SomeInt(minor)}\.?(\d*|\*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        Some(Right(VersionRange(Comparator(operator, Version(major, minor, patch, tag)))))
      // ~1
      case r"^~\s?(\d+)${SomeInt(major)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major))
        val rightVersionRange = Comparator(Operator.LT, Version(major.map(_ + 1)))
        Some(Right(VersionRange(leftVersionRange, rightVersionRange)))
      // ~1.2 | ~1.2.3
      case r"^~\s?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.?(\d*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, minor, patch, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major, minor.map(_ + 1)))
        Some(Right(VersionRange(leftVersionRange, rightVersionRange)))
      // ^1.2.3 | ^0.0 | ^0 | ^1.2.x | ^1.x | ^0.0.x | ^0.x | ~1.x
      case r"^[\^~](\d+)${SomeInt(major)}\.?(\d*|\*)${SomeInt(minor)}\.?(\d*|\*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        val desugaredMinor = minor.orElse(Some(0))
        val desugaredPatch = patch.orElse(Some(0))

        val leftVersionRange = Comparator(Operator.GTE, Version(major, desugaredMinor, desugaredPatch, tag))
        val rightVersionRange = if (major.contains(0) && minor.contains(0) && patch.isDefined) {
          Comparator(Operator.LT, Version(major, minor, patch.map(_ + 1)))
        }
        else if (major.contains(0) && minor.isDefined) {
          Comparator(Operator.LT, Version(major, minor.map(_ + 1)))
        }
        else {
          Comparator(Operator.LT, Version(major.map(_ + 1)))
        }
        Some(Right(VersionRange(leftVersionRange, rightVersionRange)))
      // latest
      case "latest" =>
        Some(Right(VersionRange(Comparator(Operator.GTE, Version(Some(0))))))
      case _ =>
        None
    }
  }

}