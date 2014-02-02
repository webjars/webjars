package models

import play.api.libs.json.Json

case class WebJar(artifactId: String, name: String, sourceUrl: String, versions: Seq[WebJarVersion])

case class WebJarVersion(number: String, numFiles: Option[Int] = None)

object WebJar {
  implicit val webJarVersionFormat = Json.format[WebJarVersion]
  implicit val webJarFormat = Json.format[WebJar]
}

object WebJarVersion {
  def cacheKey(artifactId: String, version: String): String = artifactId + "-" + version + "-files"
  
  // todo, this doesn't work on date-based versions that follow non-standard formats (e.g. ace)
  implicit object WebJarVersionOrdering extends Ordering[WebJarVersion] {
    def compare(a: WebJarVersion, b: WebJarVersion) = {
      val aParts = a.number.split(Array('.', '-'))
      val bParts = b.number.split(Array('.', '-'))
      
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
          aV.toInt compare bV.toInt
        }
        catch {
          // if we can't compare because it's not an int then just set this one to 1
          case e: NumberFormatException => 1
        }
      }
      
      // we only care about the first non-zero compare
      // if we get to the end and everything was equal then return 0
      abCompared.dropWhile(_ == 0).headOption.getOrElse(0)
    }
  }
}

