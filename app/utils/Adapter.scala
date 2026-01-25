package utils

import io.lemonlabs.uri.AbsoluteUrl
import play.api.http.{HeaderNames, Status}
import play.api.libs.ws.WSClient
import zio.{Runtime, Unsafe, ZIO, ZLayer}

import scala.concurrent.{ExecutionContext, Future}

object Adapter:
  extension [R, E <: Throwable, A] (zio: ZIO[R, E, A])
    def runToFuture(layer: ZLayer[Any, Nothing, R]): Future[A] =
      Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.runToFuture(zio.provide(layer)).future
      }


  // todo: max redirects
  extension (ws:WSClient)
    def resolveRedir(httpUrl: String)(using ExecutionContext): Future[String] = {
      ws.url(httpUrl).withFollowRedirects(false).head().flatMap { response =>
        response.status match {
          case Status.MOVED_PERMANENTLY | Status.FOUND =>
            // todo: max redirects
            response.header(HeaderNames.LOCATION).fold(Future.failed[String](new Exception("Could not get redir location"))) { location =>
              val redirUrl = if (location.startsWith("/")) {
                // relative url
                val url = AbsoluteUrl.parse(httpUrl)
                url.scheme + "://" + url.host + location
              }
              else {
                location
              }

              // keep resolving until there is an OK
              resolveRedir(redirUrl)
            }
          case Status.OK =>
            Future.successful(httpUrl)
          case _ =>
            Future.failed(new Exception(s"Could not get HEAD for url: $httpUrl"))
        }
      }
    }
