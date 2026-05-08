package webjars.utils

import zio.*
import zio.direct.*
import zio.http.*

import scala.util.{Success, Try}

trait SemVer:
  def validRange(s: String): ZIO[Scope, Throwable, Option[String]]
  def maxSatisfying(versions: Set[String], range: String): ZIO[Scope, Throwable, Option[String]]

case class SemVerLive(client: Client) extends SemVer:

  val baseUrl = "https://semver.webjars.org"

  def validRange(s: String): ZIO[Scope, Throwable, Option[String]] =
    defer:
      val url = URL.decode(s"$baseUrl/validRange?range=${java.net.URLEncoder.encode(s, "UTF-8")}").toOption.get
      val response = client.request(Request.get(url)).run
      if response.status == Status.Ok then
        Some(response.body.asString.run)
      else
        None

  def maxSatisfying(versions: Set[String], range: String): ZIO[Scope, Throwable, Option[String]] =
    val saneVersions = versions.filter(_.split('-').length <= 2)

    def fetch(versions: Set[String]): ZIO[Scope, Throwable, Option[String]] =
      defer:
        val vParams = versions.map(v => s"v=${java.net.URLEncoder.encode(v, "UTF-8")}").mkString("&")
        val rangeParam = s"range=${java.net.URLEncoder.encode(range, "UTF-8")}"
        val url = URL.decode(s"$baseUrl/maxSatisfying?$vParams&$rangeParam").toOption.get
        val response = client.request(Request.get(url)).run
        if response.status == Status.Ok then
          Some(response.body.asString.run)
        else
          None

    defer:
      if saneVersions.size > 256 then
        val maybeVersions = ZIO.foreach(saneVersions.grouped(256).toSeq)(fetch).run
        fetch(maybeVersions.flatten.toSet).run
      else
        fetch(saneVersions).run

object SemVer:
  val live: ZLayer[Client, Nothing, SemVer] = ZLayer.derive[SemVerLive]

  private def sequenceTrys[T](trySequence: Seq[? <: Try[? <: T]]): Try[Seq[T]] =
    trySequence.foldLeft(Try(Seq.empty[T])) {
      (acc, tryElement) => acc.flatMap(accSeq => tryElement.map(success => accSeq :+ success))
    }

  def toMaven(s: String): Try[String] =
    val conformedVersion = if s.isEmpty || s == "*" then ">=0" else s

    def single(s: String): Option[Either[String, String]] =
      if s.startsWith(">=") then
        Some(Left(s.replace(">=", "[")))
      else if s.startsWith(">") then
        Some(Left(s.replace(">", "(")))
      else if s.startsWith("<=") then
        Some(Right(s.replace("<=", "") + "]"))
      else if s.startsWith("<") then
        Some(Right(s.replace("<", "") + ")"))
      else
        None

    val sections = conformedVersion.replace("||", "|").split('|').toSeq
    if sections.length == 1 then
      val range = sections.head.split("\\s")
      if range.length == 1 then
        single(range.head) match
          case Some(Left(left)) => Success(left + ",)")
          case Some(Right(right)) => Success("(," + right)
          case None => Success(range.head)
      else if range.length == 2 then
        (single(range(0)), single(range(1))) match
          case (Some(Left(left)), Some(Right(right))) => Success(left + "," + right)
          case (Some(Left(left)), None) => Success(left + ",[" + range(1) + "]")
          case (None, Some(Right(right))) => Success("[" + range(0) + "]," + right)
          case _ => Success("[" + range(0) + "],[" + range(1) + "]")
      else
        throw new Exception(s"Could not parse ${sections.head}")
    else
      val tries = sections.map { group =>
        toMaven(group).map { s =>
          if !s.startsWith("(") && !s.startsWith("[") then "[" + s + "]"
          else s
        }
      }
      sequenceTrys(tries).map(_.mkString(","))
