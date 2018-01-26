package utils

import scala.language.implicitConversions
import scala.util.Try


object SemVer {

  object Operator extends Enumeration {
    type Operator = Value
    val LT = Value("<")
    val LTE = Value("<=")
    val GT = Value(">")
    val GTE = Value(">=")
    val EQ = Value("=")
  }

  // todo: some impossible states possible, e.g. major & patch
  case class Version(
                      maybeMajor: Option[Int] = None,
                      maybeMinor: Option[Int] = None,
                      maybePatch: Option[Int] = None,
                      maybeTag: Option[String] = None) {

    override def toString: String = {
      (maybeMajor, maybeMinor, maybePatch, maybeTag) match {
        case (Some(major), Some(minor), Some(patch), Some(tag)) => s"$major.$minor.$patch-$tag"
        case (Some(major), Some(minor), Some(patch), None) => s"$major.$minor.$patch"
        case (Some(major), Some(minor), None, Some(tag)) => s"$major.$minor-$tag"
        case (Some(major), Some(minor), None, None) => s"$major.$minor"
        case (Some(major), None, None, Some(tag)) => s"$major-$tag"
        case (Some(major), None, None, None) => major.toString
        case (None, None, None, Some(tag)) => tag
        case _ => ""
      }
    }

  }

  object Version {
    def apply(version: String): Version = {
      version match {
        case r"^(\d+[a-z0-9]+)?${SomeString(maybeTag)}$$" =>
          Version(None, None, None, maybeTag)
        case r"^(\d+)?${SomeInt(maybeMajor)}\.?(\d+)?${SomeInt(maybeMinor)}\.?(\d+)?${SomeInt(maybePatch)}-?(.+)?${SomeString(maybeTag)}$$" =>
          Version(maybeMajor, maybeMinor, maybePatch, maybeTag)
        case _ =>
          Version()
      }
    }
  }

  implicit object VersionOrdering extends Ordering[Version] {
    override def compare(x: Version, y: Version): Int = {
      val major = x.maybeMajor.getOrElse(0).compare(y.maybeMajor.getOrElse(0))
      if (major == 0) {
        val minor = x.maybeMinor.getOrElse(0).compare(y.maybeMinor.getOrElse(0))
        if (minor == 0) {
          val patch = x.maybePatch.getOrElse(0).compare(y.maybePatch.getOrElse(0))
          if (patch == 0) {
            x.maybeTag.getOrElse("z").compare(y.maybeTag.getOrElse("z"))
          }
          else {
            patch
          }
        }
        else {
          minor
        }
      }
      else {
        major
      }
    }
  }

  case class Comparator(operator: Operator.Operator, version: Version) {
    def includesVersion(version: Version): Boolean = {
      this.operator match {
        case Operator.GT => VersionOrdering.compare(version, this.version) > 0
        case Operator.GTE => VersionOrdering.compare(version, this.version) >= 0
        case Operator.LT => VersionOrdering.compare(version, this.version) < 0
        case Operator.LTE => VersionOrdering.compare(version, this.version) <= 0
        case Operator.EQ => VersionOrdering.compare(version, this.version) == 0
      }
    }
  }

  case class VersionRange(maybeLeft: Option[Comparator], maybeRight: Option[Comparator]) {
    def includesVersion(version: Version): Boolean = {
      maybeLeft.forall(_.includesVersion(version)) && maybeRight.forall(_.includesVersion(version))
    }
  }

  object SomeInt {
    def unapply(s: String): Option[Option[Int]] = Some(Try(s.toInt).toOption)
  }

  object SomeString {
    def unapply(s: String): Option[Option[String]] = Some(Option(s).filter(_.nonEmpty).map(_.stripPrefix("-")))
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

    versionRange match {
      case VersionRange(Some(leftRange), None) =>
        Some(leftBound(leftRange).map(_ + leftRange.version.toString + ",)").getOrElse(leftRange.version.toString))
      case VersionRange(None, Some(rightRange)) =>
        Some(rightBound(rightRange).map("(," + rightRange.version.toString + _).getOrElse(rightRange.version.toString))
      case VersionRange(Some(leftRange), Some(rightRange)) =>
        for {
          leftBound <- leftBound(leftRange)
          rightBound <- rightBound(rightRange)
        } yield leftBound + leftRange.version.toString + "," + rightRange.version.toString + rightBound
      case _ =>
        None
    }
  }

  def versionRangesToMaven(versionRanges: Seq[VersionRange]): Option[String] = {
    val convertableVersionRanges = versionRanges.flatMap(versionRangeToMaven(_).toSeq)
    if (convertableVersionRanges.isEmpty) {
      None
    }
    else {
      Some(convertableVersionRanges.mkString(","))
    }
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def convertSemVerToMaven(versionString: String): Option[String] = {

    val normalizedXRangeVersion = versionString.replace("x", "*").replace("X", "*")

    val normalizedComparatorVersion = normalizedXRangeVersion.replaceAllLiterally("> ", ">").replaceAllLiterally("< ", "<").replaceAllLiterally("= ", "=")

    val normalizedOrOrVersion = normalizedComparatorVersion.replaceAllLiterally(" || ", "||")

    val normalizedStarRangeVersion = normalizedOrOrVersion.replaceAllLiterally(".*", "")

    val normalizedVersion = normalizedStarRangeVersion

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

    normalizedVersion match {
      // 1.x || 2.1.3 || 5.0.0 - 6.0.0
      case OrOr(versionStrings) =>
        versionRangesToMaven(versionStrings.map(parseSemVerRange))

      // >1.0 <2.0
      case r"^(>=|<=|>|<)${SomeOperator(leftOperator)}([^\s>=<]+)$leftVersion\s(>=|<=|>|<)?${MaybeOperator(maybeRightOperator)}([^\s>=<]+)$rightVersion$$" =>
        val leftRange = parseSemVerRange(leftOperator + leftVersion)
        val rightRange = parseSemVerRange(maybeRightOperator.getOrElse("") + rightVersion)

        (leftRange, rightRange) match {
          case (VersionRange(Some(leftLeft), None), VersionRange(None, Some(rightRight))) =>
            // concat the left and right
            versionRangeToMaven(VersionRange(Some(leftLeft), Some(rightRight)))
          case _ =>
            versionRangesToMaven(Seq(leftRange, rightRange))
        }

      case _ =>
        versionRangeToMaven(parseSemVerRange(normalizedVersion))
    }

  }

  def parseSemVerRange(version: String): VersionRange = {

    version match {
      // No version or *
      case "" | "*" | "latest" =>
        VersionRange(Some(Comparator(Operator.GTE, Version(Some(0)))), None)
      // 1 | 1-alpha
      case r"^=?(\d+)${SomeInt(major)}(-\w+)?${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, None, None, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major.map(_ + 1), None, None, None))
        VersionRange(Some(leftVersionRange), Some(rightVersionRange))
      // 1.1 | 1.1-alpha
      case r"^=?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}(-\w+)?${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, minor, None, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major, minor.map(_ + 1), None, None))
        VersionRange(Some(leftVersionRange), Some(rightVersionRange))
      // 1.2.3 | 1.2.3-alpha | =1.2.3 | =1.2.3-alpha
      case r"^=?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.(\d+)${SomeInt(patch)}(-\w+)?${SomeString(tag)}$$" =>
        VersionRange(Some(Comparator(Operator.EQ, Version(major, minor, patch, tag))), None)
      // 1 - 2 | 1 - 2.3 | 1 - 2.3.1 | 1 - 2.3.1-alpha
      case r"^(\d+)${SomeInt(leftMajor)}\.?(\d*)${SomeInt(leftMinor)}\.?(\d*)${SomeInt(leftPatch)}-?([\w.-]*)${SomeString(leftTag)} - (\d+)${SomeInt(rightMajor)}\.?(\d*)${SomeInt(rightMinor)}\.?(\d*)${SomeInt(rightPatch)}-?([\w.-]*)${SomeString(rightTag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(leftMajor, leftMinor, leftPatch, leftTag))
        val rightVersionRange = Comparator(rightMinor.flatMap(_ => rightPatch).fold(Operator.LT)(_ => Operator.LTE) , Version(rightMajor.map(a => if (rightMinor.isEmpty) a + 1 else a), rightMinor.map(a => if (rightPatch.isEmpty) a + 1 else a), rightPatch, rightTag))
        VersionRange(Some(leftVersionRange), Some(rightVersionRange))
      // >=1.2.3 | <=1.2.3 | >1.2.3 | <1.2.3 | >=1.0.x
      case r"^(>=|<=|>|<)${SomeOperator(operator)} ?(\d+)${SomeInt(major)}\.?(\d*|\*)${SomeInt(minor)}\.?(\d*|\*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        operator match {
          case Operator.LT | Operator.LTE =>
            VersionRange(None, Some(Comparator(operator, Version(major, minor, patch, tag))))
          case _ =>
            VersionRange(Some(Comparator(operator, Version(major, minor, patch, tag))), None)
        }
      // ~1
      case r"^~\s?(\d+)${SomeInt(major)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major))
        val rightVersionRange = Comparator(Operator.LT, Version(major.map(_ + 1)))
        VersionRange(Some(leftVersionRange), Some(rightVersionRange))
      // ~1.2 | ~1.2.3
      case r"^~\s?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.?(\d*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, minor, patch, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major, minor.map(_ + 1)))
        VersionRange(Some(leftVersionRange), Some(rightVersionRange))
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
        VersionRange(Some(leftVersionRange), Some(rightVersionRange))
      case _ =>
        VersionRange(Some(Comparator(Operator.EQ, Version(version))), None)
    }
  }

  def latestInRange(versionRange: VersionRange, versions: Set[String]): Option[Version] = {
    versions.map(Version(_)).filter(versionRange.includesVersion).toSeq.sorted.lastOption
  }

}
