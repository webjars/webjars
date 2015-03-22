package utils

import play.api.Configuration
import play.api.http.{Status, HeaderNames}
import play.api.libs.json.{Json, JsValue}
import play.api.libs.ws.{WSRequestHolder, WSClient}

import scala.concurrent.{Future, ExecutionContext}

class Heroku(implicit ec: ExecutionContext, ws: WSClient, config: Configuration) {

  val apikey = config.getString("heroku.apikey").get


  private def ws(path: String): WSRequestHolder = {
    ws
      .url("https://api.heroku.com" + path)
      .withHeaders(
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

object Heroku {
  def apply(implicit ec: ExecutionContext, ws: WSClient, config: Configuration) = new Heroku()
}