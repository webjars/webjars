package utils

import javax.inject.Inject

import play.api.Configuration
import play.api.http.{HeaderNames, Status}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSClient, WSRequest}

import scala.concurrent.{ExecutionContext, Future}

class Heroku @Inject() (ws: WSClient, config: Configuration) (implicit ec: ExecutionContext) {

  lazy val apikey = config.get[String]("heroku.apikey")


  private def ws(path: String): WSRequest = {
    ws
      .url("https://api.heroku.com" + path)
      .withHttpHeaders(
        HeaderNames.ACCEPT -> "application/vnd.heroku+json; version=3",
        HeaderNames.AUTHORIZATION -> s"Bearer $apikey"
      )
  }

  def dynoCreate(app: String, attach: Boolean, command: String, size: String): Future[JsValue] = {
    val json = Json.obj(
      "attach" -> attach,
      "command" -> command,
      "size" -> size
    )

    ws(s"/apps/$app/dynos").post(json).flatMap { response =>
      response.status match {
        case Status.CREATED =>
          Future.successful(response.json)
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

}
