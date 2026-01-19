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

    def single(s: String): Option[Either[String, String]] = {
      if (s.startsWith(">=")) {
        Some(Left(s.replace(">=", "[")))
      }
      else if (s.startsWith(">")) {
        Some(Left(s.replace(">", "(")))
      }
      else if (s.startsWith("<")) {
        Some(Right(s.replace("<", "") + ")"))
      }
      else if (s.startsWith("<=")) {
        Some(Right(s.replace("<=", "") + "]"))
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
