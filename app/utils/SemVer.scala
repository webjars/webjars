package utils


object SemVer {

  def convertSemVerToMaven(versionString: String): Option[String] = {
    Some(versionString.split("\\|\\|").map(internalConvertSemVerToMaven).mkString(","))
  }

  private def internalConvertSemVerToMaven(versionString: String): String = {
    if (versionString.isEmpty || versionString.equals("*") || versionString.equals("latest")) return "[0,)"
    val version = toSemVersion(versionString)
    version match {
      case SemVersion("", "", "", "") => versionString
      case SemVersion("=", NotEmpty(staticVersion), "", "") => staticVersion
      case SemVersion("~", NotEmpty(_), "", "") => mostSignificantRange(version)
      case SemVersion("^", NotEmpty(_), "", "") => caretRange(version)
      case SemVersion(r"[<>=]+", NotEmpty(_), _, NotEmpty(_)) => rangeSet(version)
      case SemVersion(r"(?:<|>|<=|>=)", NotEmpty(_), "", "") => basicRange(version)
      case SemVersion("", NotEmpty(_), "-", NotEmpty(_)) => hyphenRange(version)
      case SemVersion("", PartialVer(_), "", "") => mostSignificantRange(version)
      case SemVersion("", NotEmpty(staticVersion), "", "") => staticVersion
    }
  }

  private def toSemVersion(versionString: String): SemVersion = {
    val normalisedVersion = versionString.replaceAll("(\\.[xX\\*])+", "").trim
    val SemVerCapture = """^([<>=~^ ]*)([a-zA-Z]*[0-9\.]+(?:-[\.\-a-zA-Z0-9]+)?)([<>=\- ]+)?([a-zA-Z]*[0-9\.]+(?:-[\.\-a-zA-Z0-9]+)?)?$""".r
    normalisedVersion match {
      case SemVerCapture(Trimmed(prefix), Trimmed(leftVersion), Trimmed(infix), Trimmed(rightVersion)) =>
        SemVersion(prefix, leftVersion, infix, rightVersion)
      case _ =>
        SemVersion("", "", "", "")
    }
  }

  private def rangeSet(version: SemVersion): String = {
    version match {
      case SemVersion(_, vl, "<", vr) => s"[$vl,$vr)"
      case SemVersion(_, vl, "", PartialVer(vr)) => s"[$vl,${incrementVersion(vr)})"
      case SemVersion(_, vl, _, vr) => s"[$vl,$vr]"
    }
  }

  private def basicRange(version: SemVersion): String = {
    version match {
      case SemVersion(">", v, _, _) => s"($v,)"
      case SemVersion(">=", v, _, _) => s"[$v,)"
      case SemVersion("<", v, _, _) => s"(,$v)"
      case SemVersion("<=", v, _, _) => s"(,$v]"
    }
  }

  private def hyphenRange(version: SemVersion): String = {
    version match {
      case SemVersion(_, vl, _, PartialVer(vr)) => s"[$vl,${incrementVersion(vr)})"
      case SemVersion(_, vl, _, vr) => s"[$vl,$vr]"
    }
  }

  def mostSignificantRange(version: SemVersion): String = {
    version match {
      case SemVersion(_, v, _, _) => s"[$v,${incrementVersion(v)})"
    }
  }

  private def caretRange(version: SemVersion): String = {
    version match {
      case SemVersion(_, ZeroVer(v), _, _) => s"[${padZeros(v)},${incrementVersion(v)})"
      case SemVersion(_, AlphaVer(v), _, _) => s"[$v,${nextAlphaVersion(v)})"
      case SemVersion(_, v, _, _) => s"[${padZeros(v)},${nextMajorVersion(v)})"
    }
  }

  private def incrementVersion(version: String): String = {
    extractVersion(version, {
      case Version(pfx, ma, SomeInt(mi), SomeInt(_), _) => s"$pfx$ma.${mi + 1}"
      case Version(pfx, ma, SomeInt(mi), _, _) => s"$pfx$ma.${mi + 1}"
      case Version(pfx, SomeInt(ma), _, _, _) => s"$pfx${ma + 1}"
    })
  }

  private def nextMajorVersion(version: String): String = {
    extractVersion(version, { case Version(prefix, SomeInt(major), _, _, _) => s"$prefix${major + 1}" })
  }

  private def nextAlphaVersion(version: String): String = {
    extractVersion(version, {
      case Version(pfx, "0", "0", SomeInt(p), _) => s"${pfx}0.0.${p + 1}"
      case Version(pfx, "0", SomeInt(mi), _, _) => s"${pfx}0.${mi + 1}"
    })
  }

  private def padZeros(version: String): String = {
    extractVersion(version, {
      case Version(pfx, ma, mi, NotEmpty(p), t) => s"$pfx$ma.$mi.$p$t"
      case Version(pfx, ma, NotEmpty(mi), _, _) => s"$pfx$ma.$mi.0"
      case Version(pfx, NotEmpty(ma), _, _, _) => s"$pfx$ma.0.0"
    })
  }

  private def extractVersion(version: String, capture: Version => String): String = {
    val VersionCapture = """([a-zA-Z]*)(\d+)\.?(\d*)\.?(\d*)((?:-[\.\-a-zA-Z0-9]+)?)""".r
    version match {
      case VersionCapture(prefix, major, minor, patch, tag) =>
        capture(Version(prefix, major, minor, patch, tag))
    }
  }

  private implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  private case class SemVersion(prefix: String, leftVersion: String, infix: String, rightVersion: String)

  private case class Version(prefix: String, major: String, minor: String, patch: String, tag: String)

  private object Trimmed {
    def unapply(string: String): Option[String] = if (string == null) Some("") else Some(string.trim)
  }

  private object NotEmpty {
    def unapply(string: String): Option[String] = if (string != null && !string.isEmpty) Some(string) else None
  }

  private object SomeInt {
    def unapply(string: String): Option[Int] = if (!string.isEmpty) Some(string.toInt) else None
  }

  private object PartialVer {
    def unapply(version: String): Option[String] = if (!version.matches("[a-zA-Z]*\\d+\\.\\d+\\.\\d+.*")) Some(version) else None
  }

  private object ZeroVer {
    def unapply(version: String): Option[String] = if (version.matches("[a-zA-Z]*(0(.0)*|0(.0)*-\\w+)")) Some(version) else None
  }

  private object AlphaVer {
    def unapply(version: String): Option[String] = if (version.matches("[a-zA-Z]*0.\\d+\\.\\d+.*")) Some(version) else None
  }

}
