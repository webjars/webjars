package utils

import io.lemonlabs.uri.{AbsoluteUrl, UrlPath}
import org.eclipse.jgit.util.Base64
import play.api.Configuration
import play.api.http.{HeaderNames, HttpVerbs, MimeTypes, Status}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSClient, WSRequest}

import javax.inject.Inject
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class GitHub @Inject() (configuration: Configuration, wsClient: WSClient, cache: Cache) (implicit ec: ExecutionContext) {

  lazy val clientId = configuration.get[String]("github.oauth.client-id")
  lazy val clientSecret = configuration.get[String]("github.oauth.client-secret")

  // primarily used in tests which break with too many concurrent requests to GitHub
  lazy val maybeAuthToken = configuration.getOptional[String]("github.auth.token")

  def ws(path: String, accessToken: String): WSRequest = {
    wsClient
      .url(s"https://api.github.com/$path")
      .withHttpHeaders(
        HeaderNames.AUTHORIZATION -> s"token $accessToken",
        HeaderNames.ACCEPT -> "application/vnd.github.v3+json"
      )
  }

  def accessToken(code: String): Future[String] = {
    val wsFuture = wsClient.url("https://github.com/login/oauth/access_token").withQueryStringParameters(
      "client_id" -> clientId,
      "client_secret" -> clientSecret,
      "code" -> code
    ).withHttpHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).execute(HttpVerbs.POST)

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
        case _ => Future.failed(ServerError(response.body, response.status))
      }
    }
  }

  def contents(accessToken: String, owner: String, repo: String, path: String): Future[String] = {
    ws(s"repos/$owner/$repo/contents/$path", accessToken).get().flatMap { response =>
      response.status match {
        case Status.OK =>
          val base64Contents = (response.json \ "content").as[String]
          Future.successful(new String(Base64.decode(base64Contents)))
        case _ => Future.failed(ServerError(response.body, response.status))
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
        case _ => Future.failed(ServerError(response.body, response.status))
      }
    }
  }

  // todo: max redirects?
  def currentUrls(url: AbsoluteUrl): Future[(AbsoluteUrl, AbsoluteUrl, AbsoluteUrl)] = {
    cache.get[(AbsoluteUrl, AbsoluteUrl, AbsoluteUrl)](url.toString, 1.day) {

      def urls(location: String) = {
        val newUrlsTry = for {
          gitHubUrl <- GitHub.gitHubUrl(location)
          sourceConnectionUri <- GitHub.gitHubGitUrl(gitHubUrl)
          issuesUrl <- GitHub.gitHubIssuesUrl(gitHubUrl)
        } yield (gitHubUrl, sourceConnectionUri, issuesUrl)

        Future.fromTry(newUrlsTry)
      }

      // GitHub can return a 429 - too many request, when running integration tests
      // Trying to auth the request to workaround
      val baseClient = wsClient.url(url.toString).withFollowRedirects(false)
      val clientMaybeWithAuth = maybeAuthToken.fold(baseClient) { authToken =>
        baseClient.withHttpHeaders(HeaderNames.AUTHORIZATION -> s"Bearer $authToken")
      }
      clientMaybeWithAuth.head().flatMap { response =>
        response.status match {
          case Status.MOVED_PERMANENTLY =>
            response.header(HeaderNames.LOCATION).fold {
              Future.failed[(AbsoluteUrl, AbsoluteUrl, AbsoluteUrl)](ServerError(s"GitHub said that $url was moved but did not provide a new location", response.status))
            } { locationString =>
              AbsoluteUrl.parseTry(locationString) match {
                case Success(locationUrl) => currentUrls(locationUrl)
                case Failure(error) => Future.failed(error)
              }
            }
          case Status.OK =>
            urls(url.toString)
          case _ =>
            Future.failed(ServerError(s"Could not get the current URL for $url because status was ${response.statusText}", response.status))
        }
      }
    }
  }

  def raw(gitHubUrl: AbsoluteUrl, tagCommitOrBranch: String, fileName: String): Future[String] = {
    val url = gitHubUrl.toString + s"/raw/$tagCommitOrBranch/$fileName"
    wsClient.url(url).get().flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.body)
        case _ => Future.failed(new Exception(response.body))
      }
    }
  }

}

object GitHub {

  def gitHubUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    if (url.apexDomain.contains("github.com")) {
      Success(
        url
          .withScheme("https")
          .withPath(UrlPath.parse(url.path.toString().stripSuffix(".git").stripSuffix("/")))
          .withFragment(None)
          .withUserInfo(None)
      )
    }
    else {
      Failure(new Error("Domain was not github.com"))
    }

  def gitHubUrl(s: String): Try[AbsoluteUrl] = AbsoluteUrl.parseTry(s).flatMap(gitHubUrl)

  def gitHubGitUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    gitHubUrl(url).flatMap { gitHubUrl =>
      gitHubUrl.path.parts match {
        case Vector(org, repo) =>
          Success(gitHubUrl.withPath(UrlPath.parse(s"/$org/$repo.git")))
        case _ =>
          Failure(new Error("Could not parse the GitHub URL"))
      }
    }

  def gitHubIssuesUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    gitHubUrl(url).map { gitHubUrl =>
      gitHubUrl.addPathPart("issues")
    }

  def maybeGitHubOrg(url: AbsoluteUrl):  Option[String] =
    url.path.parts.headOption

  def maybeGitHubRepo(url: AbsoluteUrl):  Option[String] =
    url.path.parts match {
      case _ +: repo +: _ => Some(repo)
      case _ => None
    }

}

case class UnauthorizedError(message: String) extends Exception {
  override def getMessage: String = message
}

case class ServerError(message: String, status: Int) extends Exception {
  override def getMessage: String = s"Response was $status - $message"
}
