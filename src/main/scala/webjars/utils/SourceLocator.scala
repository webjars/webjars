package webjars.utils

import zio.*
import zio.http.*

trait SourceLocator:
  def sourceUrl(url: URL): ZIO[Scope, Throwable, URL]

case class SourceLocatorLive(client: Client, git: Git) extends SourceLocator:

  def sourceUrl(url: URL): ZIO[Scope, Throwable, URL] =
    val normalized = url.scheme(Scheme.HTTPS)
    git.resolveRedirect(normalized.encode).map(URL.unsafeParse)

object SourceLocator:
  val live: ZLayer[Client & Git, Nothing, SourceLocator] = ZLayer.derive[SourceLocatorLive]
