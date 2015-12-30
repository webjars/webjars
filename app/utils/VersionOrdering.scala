package utils

import java.util.Calendar

import scala.util.Try

object VersionOrdering extends Ordering[String] {

  def unmalform(s: String): String = {
    val noPlus = s.replaceAllLiterally("+", "-")
    val fixBeta = noPlus.replaceAllLiterally("beta", ".beta.")
    val fixRc = fixBeta.replaceAllLiterally("rc", ".rc.")
    val justDots = fixRc.replaceAllLiterally("-", ".").replaceAllLiterally("..", ".")
    val betterDate = if (justDots.matches("(\\d\\d)\\.(\\d\\d)\\.(\\d\\d\\d\\d)")) {
      val parts = justDots.split('.').map(_.toInt)
      val time = Calendar.getInstance()
      time.set(parts(2), parts(0), parts(1))
      time.getTimeInMillis.toString
    } else {
      justDots
    }
    betterDate
  }

  def compare(a: String, b: String) = {

    def betaRc(s: String): String = {
      s match {
        case "dev" => "-4"
        case "alpha" => "-3"
        case "beta" => "-2"
        case "rc" => "-1"
        case _ => s
      }
    }

    def isSHA(s: String): Boolean = {
      val parts = s.split('.')
      if(parts.length == 1) {
        Try(java.lang.Long.parseLong(parts(0), 16)).isSuccess
      }
      else {
        false
      }
    }

    // determine if either of the versions might be SHA values, which we want at the beginning
    (isSHA(a), isSHA(b)) match {
      case (true, true) => a.compareTo(b)
      case (true, false) => -1
      case (false, true) => 1
      case _ =>
        // Neither is SHA. Handle common case.
        val aParts = unmalform(a).split('.').toList.map(betaRc)
        val bParts = unmalform(b).split('.').toList.map(betaRc)

        // figure out the longest one and pad each with a string 0 until the sizes match
        val longest = aParts.length max bParts.length

        val paddedAParts = aParts.padTo(longest, "0")
        val paddedBParts = bParts.padTo(longest, "0")

        // combine the two arrays into one with tuples
        val abParts = paddedAParts zip paddedBParts

        // compare all the parts as ints
        // todo: could be optimized
        val abCompared = abParts.map { case (aV, bV) =>
          try {
            aV.toLong compare bV.toLong
          }
          catch {
            case e: NumberFormatException => aV compare bV
          }
        }

        // we only care about the first non-zero compare
        // if we get to the end and everything was equal then return 0
        abCompared.dropWhile(_ == 0).headOption.getOrElse(0)
    }
  }
}
