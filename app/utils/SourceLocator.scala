package utils

import io.lemonlabs.uri.AbsoluteUrl
import play.api.http.{HeaderNames, Status}
import play.api.libs.ws.WSClient

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SourceLocator @Inject() (ws: WSClient) (implicit ec: ExecutionContext) {

  def sourceUrl(url: AbsoluteUrl): Future[AbsoluteUrl] = {
    val normalized = url.withScheme("https")
//    val urlTry = Try(new URI(normalized).toURL).recoverWith {
//      case e => Failure(new Exception(s"Could not convert provided source uri '$uri' to a URL", e))
//    }
//
//    Future.fromTry(urlTry).flatMap { url =>
      ws.url(normalized.toString).withFollowRedirects(false).get().flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful(url)
          case Status.FOUND | Status.MOVED_PERMANENTLY =>
            val maybeLocation = response.header(HeaderNames.LOCATION).flatMap { location =>
              AbsoluteUrl.parseOption(location)
            }

            maybeLocation.fold(Future.failed[AbsoluteUrl](new Exception("Could not get the redirect URL")))(Future.successful)
          case _ =>
            Future.failed(new Exception(s"Could not get a Source URL from $url"))
        }
      }
    }
//  }

}
