package utils

import io.lemonlabs.uri.{AbsoluteUrl, UrlPath}

import scala.util.{Failure, Success, Try}

object Bitbucket {
  def bitbucketUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    if (url.apexDomain.contains("bitbucket.org")) {
      Success(url.withScheme("https").withPath(UrlPath.parse(url.path.toString().stripPrefix(".git"))))
    }
    else {
      Failure(new Error("Domain was not bitbucket.org"))
    }

  def bitbucketUrl(s: String): Try[AbsoluteUrl] = AbsoluteUrl.parseTry(s).flatMap(bitbucketUrl)

  def bitbucketIssuesUrl(url: AbsoluteUrl): Try[AbsoluteUrl] = bitbucketUrl(url).map { bitbucketUrl =>
    bitbucketUrl.addPathPart("issues")
  }

}
