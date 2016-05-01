package utils

import java.util.Date

import org.apache.commons.codec.digest.{DigestUtils, HmacUtils}
import play.api.Configuration
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.WSClient

import scala.concurrent.{Future, ExecutionContext}

class Pusher(implicit ec: ExecutionContext, ws: WSClient, config: Configuration) {

  val maybeKey = config.getString("pusher.key")

  def push(channelId: String, event: String, message: String): Future[JsValue] = {

    val maybeConfig = for {
      appid <- config.getString("pusher.appid")
      key <- maybeKey
      secret <- config.getString("pusher.secret")
    } yield (appid, key, secret)

    maybeConfig.fold[Future[JsValue]] {
      Future.failed(new Exception("Could not read the Pusher config"))
    } { case (appid, key, secret) =>

      val json = Json.obj(
        "data" -> message,
        "name" -> event,
        "channel" -> channelId
      )

      val bodyMd5 = DigestUtils.md5Hex(json.toString().getBytes)

      val path = s"/apps/$appid/events"

      // keys must be in alphabetical order
      val queryStringMap: Map[String, String] = Map(
        "auth_key" -> key,
        "auth_timestamp" -> (new Date().getTime / 1000).toString,
        "auth_version" -> "1.0",
        "body_md5" -> bodyMd5
      )

      val queryString = queryStringMap.map { case (k, v) => k + "=" + v }.mkString("&")

      val stringToSign = "POST\n" + path + "\n" + queryString

      val authSignature = HmacUtils.hmacSha256Hex(secret, stringToSign)

      ws
        .url("http://api.pusherapp.com" + path + "?" + queryString + "&auth_signature=" + authSignature)
        .post(json)
        .flatMap { response =>
          response.status match {
            case Status.OK =>
              Future.successful(response.json)
            case _ =>
              Future.failed(new Exception(response.body))
          }
        }
    }
  }

}

object Pusher {
  def apply(implicit ec: ExecutionContext, ws: WSClient, config: Configuration) = new Pusher()
}