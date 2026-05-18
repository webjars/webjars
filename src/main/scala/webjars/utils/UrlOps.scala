package webjars.utils

import zio.http.URL

import scala.util.{Failure, Success, Try}

extension (companion: URL.type)
  /** Parse an absolute URL. Relative URLs are rejected — matches the
   *  scala-uri `AbsoluteUrl.parseTry` semantics that callers were written
   *  against. (`URL.decode` alone accepts both, e.g. "4.4.1" parses as a
   *  relative URL.) */
  def parseTry(s: String): Try[URL] = companion.decode(s) match
    case Right(url) if url.isAbsolute => Success(url)
    case Right(url) => Failure(IllegalArgumentException(s"Not an absolute URL: $s"))
    case Left(err) => Failure(err)

  def parseOption(s: String): Option[URL] = parseTry(s).toOption

  /** For hardcoded URL literals that should always parse. */
  def unsafeParse(s: String): URL = companion.decode(s).toOption.get
