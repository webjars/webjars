package utils

import play.api.http.{HeaderNames, Status}
import play.api.libs.ws.WSClient

import java.net.{URI, URL}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

class SourceLocator @Inject() (ws: WSClient) (implicit ec: ExecutionContext) {

  def sourceUrl(uri: URI): Future[URL] = {
    val normalized = uri.toString.replace("git://", "https://")
    val urlTry = Try(new URL(normalized)).recoverWith {
      case e => Failure(new Exception(s"Could not convert provided source uri '$uri' to a URL", e))
    }

    Future.fromTry(urlTry).flatMap { url =>
      ws.url(url.toString).withFollowRedirects(false).get().flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful(url)
          case Status.FOUND | Status.MOVED_PERMANENTLY =>
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
