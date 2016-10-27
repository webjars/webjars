package utils

import javax.inject.Inject

import org.apache.commons.codec.binary.Base64
import play.api.Configuration
import play.api.http.{HeaderNames, MimeTypes, Status}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSClient, WSRequest}
import play.api.mvc.RequestHeader
import play.api.mvc.Results.EmptyContent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions

class GitHub @Inject() (configuration: Configuration, wsClient: WSClient) {

  lazy val clientId = configuration.getString("github.oauth.client-id").get
  lazy val clientSecret = configuration.getString("github.oauth.client-secret").get

  def authUrl()(implicit request: RequestHeader): String = {
    val scope = "public_repo"
    s"https://github.com/login/oauth/authorize?client_id=$clientId&redirect_uri=$redirectUri&scope=$scope"
  }

  def redirectUri(implicit request: RequestHeader): String = {
    configuration.getString("github.oauth.redirect_uri").getOrElse {
      controllers.routes.Application.gitHubOauthCallback("").absoluteURL(request.secure).stripSuffix("?code=")
    }
  }

  def ws(path: String, accessToken: String): WSRequest = {
    wsClient
      .url(s"https://api.github.com/$path")
      .withHeaders(
        HeaderNames.AUTHORIZATION -> s"token $accessToken",
        HeaderNames.ACCEPT -> "application/vnd.github.v3+json"
      )
  }

  def accessToken(code: String)(implicit request: RequestHeader): Future[String] = {
    val wsFuture = wsClient.url("https://github.com/login/oauth/access_token").withQueryString(
      "client_id" -> clientId,
      "client_secret" -> clientSecret,
      "code" -> code
    ).withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).post(EmptyContent())

    wsFuture.flatMap { response =>
      (response.json \ "access_token").asOpt[String].fold {
        Future.failed[String](UnauthorizedError(response.body))
      } {
        Future.successful
      }
    }
  }

  def user(accessToken: String): Future[JsValue] = {
    ws("user", accessToken).get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(ServerError(response.body))
      }
    }
  }

  def contents(accessToken: String, owner: String, repo: String, path: String): Future[String] = {
    ws(s"repos/$owner/$repo/contents/$path", accessToken).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          val base64Contents = (response.json \ "content").as[String]
          Future.successful(new String(Base64.decodeBase64(base64Contents)))
        case _ => Future.failed(ServerError(response.body))
      }
    }
  }

  def createIssue(accessToken: String, owner: String, repo: String, title: String, body: String): Future[JsValue] = {
    val json = Json.obj(
      "title" -> title,
      "body" -> body
    )
    ws(s"repos/$owner/$repo/issues", accessToken).post(json).flatMap { response =>
      response.status match {
        case Status.CREATED => Future.successful(response.json)
        case _ => Future.failed(ServerError(response.body))
      }
    }
  }

}

case class UnauthorizedError(message: String) extends Exception {
  override def getMessage: String = message
}

case class ServerError(message: String) extends Exception {
  override def getMessage: String = message
}
