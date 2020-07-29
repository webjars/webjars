package utils

import fastparse.NoWhitespace._
import fastparse._

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

    // note: ordering matters cause we don't want < to match on <=
    def parse[_: P] = P(Operator.GTE.symbol | Operator.GT.symbol | Operator.LTE.symbol | Operator.LT.symbol | Operator.EQ.symbol).!.map {
      case LT.symbol => LT
      case LTE.symbol => LTE
      case GT.symbol => GT
      case GTE.symbol => GTE
      case EQ.symbol => EQ
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
    def single[_: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))

    def tag[_: P] = P("-" ~ (CharIn("a-z0-9").rep(1) ~ ("-" | ".").? ~ CharIn("a-z0-9").rep(1).?).!)

    def full[_: P] = P("v".? ~ single.? ~ ".".? ~ single.? ~ ".".? ~ single.? ~ tag.? ~ (" " | End))

    // todo: cleaner apply
    def version[_: P] = full.map { case (maybeMajor, maybeMinor, maybePatch, maybeTag) =>
      Version.apply(maybeMajor, maybeMinor, maybePatch, maybeTag)
    }

    def apply(version: String): Version = {
      if (version.isEmpty) {
        Version()
      }
      else {
        parse(version, full(_)) match {
          case Parsed.Success((maybeMajor, maybeMinor, maybePatch, maybeTag), _) =>
            Version(maybeMajor, maybeMinor, maybePatch, maybeTag)
          case Parsed.Failure(_, _, _) =>
            Version().copy(maybeTag = Some(version))
        }
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

  def convertSemVerToMaven(versionString: String): Try[String] = {
    parseSemVer(versionString).flatMap(versionRangeToMaven)
  }

  def parseSemVer(versionString: String): Try[VersionRange] = {
    val normalizedVersion = versionString
      .replace("x", "*")
      .replace("X", "*")
      .replace("> ", ">")
      .replace("< ", "<")
      .replace("= ", "=")
      .replace(".*", "")
      .replace("~ ", "~")
      .replace("#", "")

    trySequence(normalizedVersion.split(" \\|\\| ").toIndexedSeq.map(parseSemVerRange))
  }

  def parseSemVerRange(versionString: String): Try[ComparatorSet] = {

    // 1 | 1-alpha
    def major[_: P] = Version.version.filter { version =>
      version.maybeMajor.isDefined && version.maybeMinor.isEmpty && version.maybePatch.isEmpty
    } map { version =>
      val leftVersionRange = Comparator(Operator.GTE, version)
      val rightVersionRange = Comparator(Operator.LT, version.copy(maybeMajor = version.maybeMajor.map(_ + 1), maybeMinor = None, maybePatch = None, maybeTag = None))
      Set(leftVersionRange, rightVersionRange)
    }

    // 1.1 | 1.1-alpha
    def minor[_: P] = Version.version.filter { version =>
      version.maybeMajor.isDefined && version.maybeMinor.isDefined && version.maybePatch.isEmpty
    } map { version =>
      val leftVersionRange = Comparator(Operator.GTE, version)
      val rightVersionRange = Comparator(Operator.LT, version.copy(maybeMinor = version.maybeMinor.map(_ + 1), maybePatch = None, maybeTag = None))
      Set(leftVersionRange, rightVersionRange)
    }

    // 1.2.3 | 1.2.3-alpha | =1.2.3 | =1.2.3-alpha
    def patch[_: P] = (Operator.EQ.symbol.? ~ Version.version).filter { version =>
      version.maybeMajor.isDefined && version.maybeMinor.isDefined && version.maybePatch.isDefined
    } map { version =>
      Set(Comparator(Operator.EQ, version))
    }

    def version[_: P] = Version.version.filter { version =>
      version.maybeMajor.isDefined || version.maybeMinor.isDefined || version.maybePatch.isDefined
    }

    // <1 | >1.0 | <=1.0.0
    def operator[_: P] = (Operator.parse ~ version).map { case (operator, version) =>
      Set(Comparator(operator, version))
    }

    def range[_: P] = major | minor | patch | operator

    // >1.0 <2.0
    def rangeSet[_: P] = (range ~ range).map { case (left, right) =>
      left ++ right
    }

    // todo: some duplicate logic
    // 1 - 2 | 1 - 2.3 | 1 - 2.3.1 | 1 - 2.3.1-alpha
    def hyphenRange[_: P] = (version ~ "- " ~ version).map { case (left, right) =>
      val normalizedLeft = left.copy(
        maybeMajor = Some(left.maybeMajor.getOrElse(0)),
        maybeMinor = Some(left.maybeMinor.getOrElse(0)),
        maybePatch = Some(left.maybePatch.getOrElse(0)),
      )
      val leftVersionRange = Comparator(Operator.GTE, normalizedLeft)

      val rightOperator = right.maybeMinor.flatMap(_ => right.maybePatch).fold[Operator](Operator.LT)(_ => Operator.LTE)
      val normalizedRight = right.copy(
        maybeMajor = right.maybeMajor.map { major => right.maybeMinor.fold(major + 1)(_ => major) }, // if the minor is empty, bump the major
        maybeMinor = right.maybeMinor.map { minor => right.maybePatch.fold(minor + 1)(_ => minor) }, // if the patch is empty, bump the minor
      )
      val rightVersionRange = Comparator(rightOperator, normalizedRight)

      Set(leftVersionRange, rightVersionRange)
    }

    // todo: some duplicate logic
    // ^1.2.3 | ^0.0 | ^0 | ^1.2.x | ^1.x | ^0.0.x | ^0.x
    def carrot[_: P] = ("^" ~ version).map { version =>
      val desugaredMinor = version.maybeMinor.orElse(Some(0))
      val desugaredPatch = version.maybePatch.orElse(Some(0))

      val leftVersion = version.copy(maybeMinor = desugaredMinor, maybePatch = desugaredPatch)

      val rightVersion = if (version.maybeMajor.contains(0) && version.maybeMinor.contains(0) && version.maybePatch.isDefined) {
        version.copy(maybePatch = version.maybePatch.map(_ + 1), maybeTag = None)
      }
      else if (version.maybeMajor.contains(0) && version.maybeMinor.isDefined) {
        version.copy(maybeMinor = version.maybeMinor.map(_ + 1), maybePatch = None, maybeTag = None)
      }
      else {
        version.copy(maybeMajor = version.maybeMajor.map(_ + 1), maybeMinor = None, maybePatch = None, maybeTag = None)
      }

      Set(Comparator(Operator.GTE, leftVersion), Comparator(Operator.LT, rightVersion))
    }

    // todo: some duplicate logic
    // ~1.2.3 | ~0.0 | ~0 | ~1.2.x | ~1.x | ~0.0.x | ~0.x
    def tilde[_: P] = ("~" ~ version).map { version =>
      val rightVersion = if (version.maybePatch.isDefined || version.maybeMinor.isDefined) {
        version.copy(maybeMinor = version.maybeMinor.map(_ + 1), maybePatch = None, maybeTag = None)
      }
      else {
        version.copy(maybeMajor = version.maybeMajor.map(_ + 1), maybeMinor = None, maybePatch = None, maybeTag = None)
      }

      Set(Comparator(Operator.GTE, version), Comparator(Operator.LT, rightVersion))
    }

    def string[_: P] = CharIn("a-z0-9").rep(1).!.map { s =>
      Set(Comparator(Operator.EQ, Version(None, None, None, Some(s))))
    }

    def all[_:P] = P(Start ~ (rangeSet | hyphenRange | range | carrot | tilde | string ~ End))

    if (versionString.isEmpty || versionString == "*" || versionString == "latest") {
      Success(Set(Comparator(Operator.GTE, Version(Some(0)))))
    }
    else {
      parse(versionString, all(_)).fold({ case (_, _, _) =>
        Failure(new Exception(s"Could not convert version $versionString to SemVer"))
      }, { case (comparatorSet, _) =>
        Success(comparatorSet)
      })
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
