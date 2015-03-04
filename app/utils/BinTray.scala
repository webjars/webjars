package utils

import play.api.libs.ws.WSAPI

import scala.concurrent.ExecutionContext

class BinTray(implicit ec: ExecutionContext, ws: WSAPI) {

  val BASE_URL = "https://bintray.com/api/v1"



}

object BinTray {
  def apply(implicit ec: ExecutionContext, ws: WSAPI) = new Bower()
}