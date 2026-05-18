package webjars.utils

import zio.http.{Path, Scheme, URL}

import scala.util.{Failure, Success, Try}

object Bitbucket:
  def bitbucketUrl(url: URL): Try[URL] =
    if url.host.exists(_.endsWith("bitbucket.org")) then
      val cleaned = url.path.encode.stripPrefix(".git")
      Success(url.scheme(Scheme.HTTPS).path(cleaned))
    else
      Failure(new Error("Domain was not bitbucket.org"))

  def bitbucketUrl(s: String): Try[URL] = URL.parseTry(s).flatMap(bitbucketUrl)

  def bitbucketIssuesUrl(url: URL): Try[URL] = bitbucketUrl(url).map(_ / "issues")
