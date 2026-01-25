package utils

import io.lemonlabs.uri.AbsoluteUrl
import play.api.http.{HeaderNames, Status}
import play.api.libs.ws.WSClient
import utils.Adapter.*

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SourceLocator @Inject() (ws: WSClient) (using ec: ExecutionContext):

  def sourceUrl(url: AbsoluteUrl): Future[AbsoluteUrl] =
    val normalized = url.withScheme("https")
    ws.resolveRedir(normalized.toString).map(AbsoluteUrl.parse)
