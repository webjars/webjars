package utils

import play.api.http.Status
import play.api.libs.ws.WSClient

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

class SemVer @Inject() (val ws: WSClient) (using ec: ExecutionContext) {

  val baseUrl = "https://semver.webjars.org"

  def validRange(s: String): Future[Option[String]] = {
    ws.url(baseUrl + "/validRange").withQueryStringParameters("range" -> s).get().map { response =>
      Option.when(response.status == Status.OK)(response.body)
    }
  }

  def maxSatisfying(versions: Set[String], range: String): Future[Option[String]] = {
    val saneVersions = versions.filter(_.split('-').length <= 2) // ie 18.3.0-canary-1219d57fc-20240201,

    def fetch(versions: Set[String]): Future[Option[String]] = {
      val vParams = versions.map("v" -> _).toSeq
      ws.url(baseUrl + "/maxSatisfying").withQueryStringParameters(vParams*).addQueryStringParameters("range" -> range).get().map { response =>
        Option.when(response.status == Status.OK)(response.body)
      }
    }

    if (saneVersions.size > 256) {
      // partition the versions so we don't exceed the query string limit
      Future.traverse(saneVersions.grouped(256)) { versions =>
        fetch(versions)
      }.flatMap { maybeVersions =>
        // out of the possible versions matching, re-find the maxSatisfying
        val versions = maybeVersions.flatten.toSet
        fetch(versions)
      }
    }
    else {
      // the partitioned version fetches at least twice, for smaller sets, just fetch once
      fetch(saneVersions)
    }
  }

}

object SemVer {

  private def sequenceTrys[T](trySequence: Seq[? <: Try[? <: T]]): Try[Seq[T]] = {
    trySequence.foldLeft(Try(Seq.empty[T])) {
      (acc, tryElement) => acc.flatMap(accSeq => tryElement.map(success => accSeq :+ success))
    }
  }

  def toMaven(s: String): Try[String] = {
    val conformedVersion = if (s.isEmpty || s == "*") {
      ">=0"
    }
    else {
      s
    }

    // Convert NPM semver -0 suffix to Maven-compatible high version number
    // NPM uses <X.Y.Z-0 to exclude all pre-releases, but Maven's version ordering
    // treats X.Y.Z-alpha < X.Y.Z-0 == X.Y.Z, so we can't use X.Y.Z as upper bound.
    // Instead, we convert <X.Y.Z-0 to (X-1).999999.999999 to stay below pre-releases.
    // Examples: <4.0.0-0 -> 3.999999.999999, <1.3.0-0 -> 1.2.999999, <0.0.4-0 -> 0.0.3
    // Edge case: <0.0.0-0 would require decrementing major from 0 to -1, which is invalid.
    // In this case, the function returns None to indicate conversion failure.
    def convertNpmUpperBound(version: String): Option[String] = {
      if (!version.endsWith("-0")) {
        Some(version)
      } else {
        // Remove -0 suffix and parse version parts
        val versionWithoutSuffix = version.dropRight(2)
        val parts = versionWithoutSuffix.split("\\.").map(_.toIntOption.getOrElse(0)).toList
        
        // Find the last non-zero part (or use major if all zero)
        val decrementIndex = parts.lastIndexWhere(_ > 0) match {
          case -1 => 0  // All zeros, attempt to decrement major (will fail validation and return None)
          case idx => idx
        }
        
        // Build new version: decrement at index, set 999999 for all parts after
        val newParts = parts.zipWithIndex.map { case (value, idx) =>
          if (idx < decrementIndex) {
            value.toString
          } else if (idx == decrementIndex) {
            (value - 1).toString
          } else {
            "999999"
          }
        }
        
        val result = newParts.mkString(".")
        // Validate that we didn't create a negative version (e.g., -1.999999.999999)
        // This can only happen if we're decrementing the major version from 0
        if (newParts.head.startsWith("-")) {
          None  // Invalid: attempting to decrement below 0
        } else {
          Some(result)
        }
      }
    }

    def single(s: String): Option[Either[String, String]] = {
      if (s.startsWith(">=")) {
        Some(Left(s.replace(">=", "[")))
      }
      else if (s.startsWith(">")) {
        Some(Left(s.replace(">", "(")))
      }
      else if (s.startsWith("<=")) {
        val versionStr = s.replace("<=", "")
        convertNpmUpperBound(versionStr) match {
          case Some(version) => Some(Right(version + "]"))
          case None => None
        }
      }
      else if (s.startsWith("<")) {
        val versionStr = s.replace("<", "")
        convertNpmUpperBound(versionStr) match {
          case Some(version) =>
            // Use inclusive bracket ] since we computed the max version in the range
            Some(Right(version + "]"))
          case None => None
        }
      }
      else {
        None
      }
    }

    // replace because I don't want regex
    val sections = conformedVersion.replace("||", "|").split('|').toSeq
    if (sections.length == 1) {
      val range = sections.head.split("\\s")
      if (range.length == 1) {
        single(range.head) match {
          case Some(Left(left)) =>
            Success(left + ",)")
          case Some(Right(right)) =>
            Success("(," + right)
          case None =>
            Success(range.head)
        }
      }
      else if (range.length == 2) {
        (single(range(0)), single(range(1))) match {
          case (Some(Left(left)), Some(Right(right))) =>
            Success(left + "," + right)
          case (Some(Left(left)), None) =>
            Success(left + ",[" + range(1) + "]")
          case (None, Some(Right(right))) =>
            Success("[" + range(0) + "]," + right)
          case _ =>
            Success("[" + range(0) + "],[" + range(1) + "]")
        }
      }
      else {
        throw new Exception(s"Could not parse ${sections.head}")
      }
    }
    else {
      val tries = sections.map { group =>
        toMaven(group).map { s =>
          if (!s.startsWith("(") && !s.startsWith("[")) {
            // wrap non-ranged as ranges
            "[" + s + "]"
          }
          else {
            s
          }
        }
      }

      sequenceTrys(tries).map(_.mkString(","))
    }
  }

}
