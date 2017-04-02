package utils

import java.net.{URI, URL}

import scala.util.Try

object Bitbucket {

  def bitbucketUrl(url: URL): Try[URL] = Try(new URL(url.getProtocol, url.getHost, url.getPath.stripSuffix(".git"))).filter(_.getHost.stripPrefix("www.") == "bitbucket.org")

  def bitbucketUrl(uri: URI): Try[URL] = Try(new URL("https", uri.getHost, uri.getPath)).flatMap(bitbucketUrl)

  def bitbucketUrl(s: String): Try[URL] = Try(new URI(s)).flatMap(bitbucketUrl)

  def bitbucketIssuesUrl(url: URL): Try[URL] = bitbucketUrl(url).flatMap { bitbucketUrl =>
    Try(new URL(bitbucketUrl.getProtocol, bitbucketUrl.getHost, bitbucketUrl.getPath + "/issues"))
  }

  def bitbucketIssuesUrl(uri: URI): Try[URL] = bitbucketUrl(uri).flatMap(bitbucketIssuesUrl)

}
