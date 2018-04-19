package utils

import scala.util.{Failure, Success, Try}


object SemVer {

  sealed trait Operator {
    val symbol: String
    override def toString: String = symbol
  }

  object Operator {

    case object LT extends Operator {
      val symbol = "<"
    }

    case object LTE extends Operator {
      val symbol = "<="
    }

    case object GT extends Operator {
      val symbol = ">"
    }

    case object GTE extends Operator {
      val symbol = ">="
    }

    case object EQ extends Operator {
      val symbol = "="
    }

    def parse(s: String): Option[Operator] = {
      s match {
        case LT.symbol => Some(LT)
        case LTE.symbol => Some(LTE)
        case GT.symbol => Some(GT)
        case GTE.symbol => Some(GTE)
        case EQ.symbol => Some(EQ)
        case _ => None
      }
    }
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
        case r"^v?(\d+)?${SomeInt(maybeMajor)}\.?(\d+)?${SomeInt(maybeMinor)}\.?(\d+)?${SomeInt(maybePatch)}-?(.+)?${SomeString(maybeTag)}$$" =>
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

  case class Comparator(operator: Operator, version: Version) {
    def includesVersion(versionToCheck: Version): Boolean = {
      operator match {
        case Operator.GT => VersionOrdering.gt(versionToCheck, version)
        case Operator.GTE => VersionOrdering.gteq(versionToCheck, version)
        case Operator.LT => VersionOrdering.lt(versionToCheck, version)
        case Operator.LTE => VersionOrdering.lteq(versionToCheck, version)
        case Operator.EQ => VersionOrdering.equiv(versionToCheck, version)
      }
    }
  }

  type ComparatorSet = Set[Comparator]

  implicit class ComparatorSetExtensions(val comparatorSet: ComparatorSet) extends AnyVal {
    def includesVersion(version: Version): Boolean = {
      comparatorSet.forall(_.includesVersion(version))
    }
  }

  type VersionRange = Seq[ComparatorSet]

  implicit class VersionRangeExtensions(val versionRange: VersionRange) extends AnyVal {
    def includesVersion(version: Version): Boolean = {
      versionRange.map(_.includesVersion(version)).reduce(_ || _)
    }
  }

  object SomeInt {
    def unapply(s: String): Option[Option[Int]] = Some(Try(s.toInt).toOption)
  }

  object SomeString {
    def unapply(s: String): Option[Option[String]] = Some(Option(s).filter(_.nonEmpty).map(_.stripPrefix("-")))
  }

  object SomeOperator {
    def unapply(s: String): Option[Operator] = Operator.parse(s)
  }

  object MaybeOperator {
    def unapply(s: String): Option[Option[Operator]] = Some(Operator.parse(s))
  }

  // this is imperative crap
  def comparatorSetToMaven(comparatorSet: ComparatorSet): Try[String] = {
    if (comparatorSet.size == 1) {
      val comparator = comparatorSet.head
      comparator.operator match {
        case Operator.GT => Success("(" + comparator.version.toString + ",)" )
        case Operator.GTE => Success("[" + comparator.version.toString + ",)" )
        case Operator.LT => Success("(," + comparator.version.toString + ")" )
        case Operator.LTE => Success("(," + comparator.version.toString + "]" )
        case Operator.EQ => Success("[" + comparator.version.toString + "]" )
      }
    }
    else {
      if (comparatorSet.count(_.operator == Operator.EQ) > 1) {
        Failure(new Exception("Can not create an intersection with multiple equals in the comparator set"))
      }
      else if (comparatorSet.count(_.operator == Operator.EQ) == 1) {
        val eqComparator = comparatorSet.find(_.operator == Operator.EQ).get
        val otherComparators = comparatorSet.filterNot(_.operator == Operator.EQ)

        val eqComparatorFits = otherComparators.foldLeft(true) { case (fits, comparator) =>
          fits && comparator.includesVersion(eqComparator.version)
        }

        if (eqComparatorFits) {
          comparatorSetToMaven(Set(eqComparator))
        }
        else {
          Failure(new Exception("Intersection of the comparator set was empty"))
        }
      }
      else {
        val lowerBounds = comparatorSet.filter { comparator =>
          comparator.operator == Operator.GT || comparator.operator == Operator.GTE
        }

        val upperBounds = comparatorSet.filter { comparator =>
          comparator.operator == Operator.LT || comparator.operator == Operator.LTE
        }

        if (lowerBounds.isEmpty && upperBounds.nonEmpty) {
          val upperBound = upperBounds.minBy(_.version)
          comparatorSetToMaven(Set(upperBound))
        }
        else if (upperBounds.isEmpty && lowerBounds.nonEmpty) {
          val lowerBound = lowerBounds.maxBy(_.version)
          comparatorSetToMaven(Set(lowerBound))
        }
        else if (upperBounds.nonEmpty && lowerBounds.nonEmpty) {
          val lowerBound = lowerBounds.maxBy(_.version)
          val upperBound = upperBounds.minBy(_.version)

          if (VersionOrdering.gt(lowerBound.version, upperBound.version)) {
            Failure(new Exception("No valid intersection of the comparator set"))
          }
          else {

            val tryLowerBound = lowerBound.operator match {
              case Operator.GT => Success("(" + lowerBound.version.toString)
              case Operator.GTE => Success("[" + lowerBound.version.toString)
              case _ => Failure(new Exception("Could not convert the lower bound to Maven"))
            }

            val tryUpperBound = upperBound.operator match {
              case Operator.LT => Success(upperBound.version.toString + ")")
              case Operator.LTE => Success(upperBound.version.toString + "]")
              case _ => Failure(new Exception("Could not convert the upper bound to Maven"))
            }

            for {
              lowerBoundString <- tryLowerBound
              upperBoundString <- tryUpperBound
            } yield lowerBoundString + "," + upperBoundString
          }
        }
        else {
          Failure(new Exception("Could not convert the comparator set to Maven"))
        }
      }
    }
  }

  private def trySequence[T](trys: Seq[Try[T]]): Try[Seq[T]] = {
    trys.find(_.isFailure).fold[Try[Seq[T]]](Success(trys.map(_.get))) { failure =>
      failure.map(Seq(_))
    }
  }

  def versionRangeToMaven(versionRange: VersionRange): Try[String] = {
    trySequence(versionRange.map(comparatorSetToMaven)).map(_.mkString(","))
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def convertSemVerToMaven(versionString: String): Try[String] = {
    parseSemVer(versionString).flatMap(versionRangeToMaven)
  }

  def parseSemVer(versionString: String): Try[VersionRange] = {
    val normalizedVersion = versionString
      .replaceAllLiterally("x", "*")
      .replaceAllLiterally("X", "*")
      .replaceAllLiterally("> ", ">")
      .replaceAllLiterally("< ", "<")
      .replaceAllLiterally("= ", "=")
      .replaceAllLiterally(" || ", "||")
      .replaceAllLiterally(".*", "")
      .replaceAllLiterally("~ ", "~")
      .replaceAllLiterally("#", "")

    val parseTrys = normalizedVersion.split("\\|\\|").map(parseSemVerRange).toSeq

    trySequence(parseTrys)
  }

  def parseSemVerRange(version: String): Try[ComparatorSet] = {
    version match {
      // No version or *
      case "" | "*" | "latest" =>
        Success(Set(Comparator(Operator.GTE, Version(Some(0)))))

      // 1 | 1-alpha
      case r"^=?(\d+)${SomeInt(major)}(-\w+)?${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, None, None, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major.map(_ + 1), None, None, None))
        Success(Set(leftVersionRange, rightVersionRange))

      // 1.1 | 1.1-alpha
      case r"^=?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}(-\w+)?${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, minor, None, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major, minor.map(_ + 1), None, None))
        Success(Set(leftVersionRange, rightVersionRange))

      // 1.2.3 | 1.2.3-alpha | =1.2.3 | =1.2.3-alpha
      case r"^=?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.(\d+)${SomeInt(patch)}([\w-.]+)?${SomeString(tag)}$$" =>
        Success(Set(Comparator(Operator.EQ, Version(major, minor, patch, tag))))

      // >1.0 <2.0
      case r"^([^\s]+)$leftVersion\s([^\s]+)$rightVersion$$" =>
        val maybeLeftRange = parseSemVerRange(leftVersion)
        val maybeRightRange = parseSemVerRange(rightVersion)

        for {
          leftRange <- maybeLeftRange
          rightRange <- maybeRightRange
        } yield leftRange ++ rightRange

      // 1 - 2 | 1 - 2.3 | 1 - 2.3.1 | 1 - 2.3.1-alpha
      case r"^(\d+)${SomeInt(leftMajor)}\.?(\d*)${SomeInt(leftMinor)}\.?(\d*)${SomeInt(leftPatch)}-?([\w.-]*)${SomeString(leftTag)} - (\d+)${SomeInt(rightMajor)}\.?(\d*)${SomeInt(rightMinor)}\.?(\d*)${SomeInt(rightPatch)}-?([\w.-]*)${SomeString(rightTag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(Some(leftMajor.getOrElse(0)), Some(leftMinor.getOrElse(0)), Some(leftPatch.getOrElse(0)), leftTag))
        val rightVersionRange = Comparator(rightMinor.flatMap(_ => rightPatch).fold[Operator](Operator.LT)(_ => Operator.LTE) , Version(rightMajor.map(a => if (rightMinor.isEmpty) a + 1 else a), rightMinor.map(a => if (rightPatch.isEmpty) a + 1 else a), rightPatch, rightTag))
        Success(Set(leftVersionRange, rightVersionRange))

      // >=1.2.3 | <=1.2.3 | >1.2.3 | <1.2.3 | >=1.0.x
      case r"^(>=|<=|>|<)${SomeOperator(operator)} ?(\d+)${SomeInt(major)}\.?(\d*|\*)${SomeInt(minor)}\.?(\d*|\*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        Success(Set(Comparator(operator, Version(major, minor, patch, tag))))

      // ~1
      case r"^~\s?(\d+)${SomeInt(major)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major))
        val rightVersionRange = Comparator(Operator.LT, Version(major.map(_ + 1)))
        Success(Set(leftVersionRange, rightVersionRange))

      // ~1.2 | ~1.2.3
      case r"^~\s?(\d+)${SomeInt(major)}\.(\d+)${SomeInt(minor)}\.?(\d*)${SomeInt(patch)}-?([\w.-]*)${SomeString(tag)}$$" =>
        val leftVersionRange = Comparator(Operator.GTE, Version(major, minor, patch, tag))
        val rightVersionRange = Comparator(Operator.LT, Version(major, minor.map(_ + 1)))
        Success(Set(leftVersionRange, rightVersionRange))

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
        Success(Set(leftVersionRange, rightVersionRange))

      // non-semver things like git hashes
      case r"^([\w\d]+)${SomeString(maybeVersion)}$$" =>
        maybeVersion.fold[Try[ComparatorSet]](Failure(new Exception("sCould not convert version $version to SemVer"))) { version =>
          Success(Set(Comparator(Operator.EQ, Version(version))))
        }

      case _ =>
        Failure(new Exception(s"Could not convert version $version to SemVer"))
    }
  }

  def latestInRange(versionRange: VersionRange, versions: Set[String]): Option[String] = {
    versions
      .map { v => Version(v) -> v }
      .filter { case (v, _) => versionRange.includesVersion(v) }
      .toSeq
      .sorted
      .lastOption
      .map(_._2)
  }

}
