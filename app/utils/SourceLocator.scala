package utils

import java.net.{URI, URL}
import javax.inject.Inject

import play.api.http.{HeaderNames, HttpVerbs, Status}
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class SourceLocator @Inject() (ws: WSClient) (implicit ec: ExecutionContext) {

  def sourceUrl(uri: URI): Future[URL] = {
    Future.fromTry(Try(uri.toURL)).flatMap { url =>
      ws.url(url.toString).withFollowRedirects(false).get().flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful(url)
          case Status.FOUND =>
            val maybeLocation = response.header(HeaderNames.LOCATION).flatMap { location =>
              Try(new URL(location)).toOption
            }

            maybeLocation.fold(Future.failed[URL](new Exception("Could not get the redirect URL")))(Future.successful)
          case _ =>
            Future.failed(new Exception(s"Could not get a Source URL from $uri"))
        }
      }
    }
  }

}
