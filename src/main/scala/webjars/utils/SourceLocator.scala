package webjars.utils

import io.lemonlabs.uri.AbsoluteUrl
import zio.*
import zio.http.*

trait SourceLocator:
  def sourceUrl(url: AbsoluteUrl): ZIO[Scope, Throwable, AbsoluteUrl]

case class SourceLocatorLive(client: Client, git: Git) extends SourceLocator:

  def sourceUrl(url: AbsoluteUrl): ZIO[Scope, Throwable, AbsoluteUrl] =
    val normalized = url.withScheme("https")
    git.resolveRedirect(normalized.toString).map(AbsoluteUrl.parse)

object SourceLocator:
  val live: ZLayer[Client & Git, Nothing, SourceLocator] = ZLayer.derive[SourceLocatorLive]
