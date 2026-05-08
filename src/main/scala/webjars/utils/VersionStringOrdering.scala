package webjars.utils

import java.util.Calendar
import scala.util.Try

object VersionStringOrdering extends Ordering[String]:

  def unmalform(versionString: String): String =
    val noPlus = (s: String) => s.replace("+", "-")
    val fixAlpha = (s: String) => s.replace("alpha", ".alpha.")
    val fixBeta = (s: String) => s.replace("beta", ".beta.")
    val fixRc = (s: String) => s.replace("rc", ".rc.")
    val justDots = (s: String) => s.replace("-", ".").replace("..", ".")
    val betterDate = (s: String) =>
      if s.matches("(\\d\\d)\\.(\\d\\d)\\.(\\d\\d\\d\\d)") then
        val parts = s.split('.').map(_.toInt)
        val time = Calendar.getInstance()
        time.set(parts(2), parts(0), parts(1))
        time.getTimeInMillis.toString
      else
        s

    val transforms = Seq(noPlus, fixAlpha, fixBeta, fixRc, justDots, betterDate)
    transforms.foldLeft(versionString)((a, f) => f(a))

  def compare(a: String, b: String): Int =

    def betaRc(s: String): String =
      s match
        case "dev" => "-4"
        case "alpha" => "-3"
        case "beta" => "-2"
        case "rc" => "-1"
        case _ => s

    def isSHA(s: String): Boolean =
      val parts = s.split('.')
      if parts.length == 1 then
        Try(java.lang.Long.parseLong(parts(0), 16)).isSuccess
      else
        false

    (isSHA(a), isSHA(b)) match
      case (true, true) => a.compareTo(b)
      case (true, false) => -1
      case (false, true) => 1
      case _ =>
        val aParts = unmalform(a).split('.').toList.map(betaRc)
        val bParts = unmalform(b).split('.').toList.map(betaRc)
        val longest = aParts.length max bParts.length
        val paddedAParts = aParts.padTo(longest, "0")
        val paddedBParts = bParts.padTo(longest, "0")
        val abParts = paddedAParts zip paddedBParts
        val abCompared = abParts.map { case (aV, bV) =>
          try aV.toLong compare bV.toLong
          catch case _: NumberFormatException => aV compare bV
        }
        abCompared.dropWhile(_ == 0).headOption.getOrElse(0)
