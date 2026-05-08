package webjars.utils

import io.lemonlabs.uri.{AbsoluteUrl, UrlPath}
import webjars.config.AppConfig
import webjars.utils.Deployable.Version
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

import scala.util.{Failure, Success, Try}

trait GitHub:
  def maybeAuthToken: Option[String]
  def currentUrls(url: AbsoluteUrl): ZIO[Scope, Throwable, (AbsoluteUrl, AbsoluteUrl, AbsoluteUrl)]
  def raw(gitHubUrl: AbsoluteUrl, tagCommitOrBranch: String, fileName: String): ZIO[Scope, Throwable, String]
  def allPages[T](path: String, accumulator: Set[T] = Set.empty[T])(mapFunction: Response => ZIO[Scope, Throwable, Set[T]]): ZIO[Scope, Throwable, Set[T]]
  def tags(repo: String): ZIO[Scope, Throwable, Set[Version]]

case class GitHubLive(client: Client, config: AppConfig, cache: Cache) extends GitHub:

  lazy val maybeAuthToken: Option[String] = config.githubAuthToken

  private def wsRequest(path: String, accessToken: String): Request =
    Request.get(URL.decode(s"https://api.github.com/$path").toOption.get)
      .addHeader(Header.Authorization.Bearer(accessToken))
      .addHeader(Header.Accept(MediaType.application.json))

  def currentUrls(url: AbsoluteUrl): ZIO[Scope, Throwable, (AbsoluteUrl, AbsoluteUrl, AbsoluteUrl)] =
    cache.get[(AbsoluteUrl, AbsoluteUrl, AbsoluteUrl)](url.toString, 1.day) {
      def urls(location: String) =
        val newUrlsTry = for
          gitHubUrl <- GitHub.gitHubUrl(location)
          sourceConnectionUri <- GitHub.gitHubGitUrl(gitHubUrl)
          issuesUrl <- GitHub.gitHubIssuesUrl(gitHubUrl)
        yield (gitHubUrl, sourceConnectionUri, issuesUrl)
        ZIO.fromTry(newUrlsTry)

      val baseRequest = Request.get(URL.decode(url.toString).toOption.get)
        .addHeader(Header.Custom("Follow-Redirects", "false"))
      val reqWithAuth = maybeAuthToken.fold(baseRequest)(token =>
        baseRequest.addHeader(Header.Authorization.Bearer(token))
      )

      defer:
        val response = client.request(reqWithAuth).run
        response.status match
          case Status.MovedPermanently =>
            response.header(Header.Location) match
              case Some(location) =>
                AbsoluteUrl.parseTry(location.url.encode) match
                  case Success(locationUrl) => currentUrls(locationUrl).run
                  case Failure(error) => ZIO.fail(error).run
              case None =>
                ZIO.fail(ServerError(s"GitHub said that $url was moved but did not provide a new location", response.status.code)).run
          case Status.Ok =>
            urls(url.toString).run
          case _ =>
            ZIO.fail(ServerError(s"Could not get the current URL for $url because status was ${response.status.code}", response.status.code)).run
    }

  def raw(gitHubUrl: AbsoluteUrl, tagCommitOrBranch: String, fileName: String): ZIO[Scope, Throwable, String] =
    defer:
      val rawUrl = s"https://raw.githubusercontent.com${gitHubUrl.path}/$tagCommitOrBranch/$fileName"
      val baseRequest = Request.get(URL.decode(rawUrl).toOption.get)
      val request = maybeAuthToken.fold(baseRequest)(token =>
        baseRequest.addHeader(Header.Authorization.Bearer(token))
      )
      val response = client.request(request).run
      response.status match
        case Status.Ok => response.body.asString.run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(body)).run

  def allPages[T](path: String, accumulator: Set[T] = Set.empty[T])(mapFunction: Response => ZIO[Scope, Throwable, Set[T]]): ZIO[Scope, Throwable, Set[T]] =
    val request = maybeAuthToken.fold(
      Request.get(URL.decode(s"https://api.github.com/$path").toOption.get)
    )(accessToken =>
      wsRequest(path, accessToken)
    )

    defer:
      val response = client.request(request).run
      response.status match
        case Status.Ok =>
          val these = mapFunction(response).run
          val all = accumulator ++ these
          val maybeLink = ZIO.succeed {
            response.rawHeader("Link").flatMap { link =>
              link.split(", ").find(_.contains("rel=\"next\"")).map { link =>
                link.stripPrefix("<https://api.github.com/").stripSuffix(">; rel=\"next\"")
              }
            }
          }.run
          maybeLink match
            case Some(nextPath) => allPages(nextPath, all)(mapFunction).run
            case None => all
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(ServerError(body, response.status.code)).run

  def tags(repo: String): ZIO[Scope, Throwable, Set[Version]] =
    import zio.json.ast.Json
    allPages(s"repos/$repo/tags") { response =>
      defer:
        val body = response.body.asString.run
        val tags = ZIO.fromEither(body.fromJson[List[Json]].left.map(new Exception(_))).run
        tags.flatMap(_.asObject.flatMap(_.get("name")).flatMap(_.asString)).toSet
    }

object GitHub:

  val live: ZLayer[Client & AppConfig & Cache, Nothing, GitHub] = ZLayer.derive[GitHubLive]

  def gitHubUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    if url.apexDomain.contains("github.com") then
      val path = UrlPath.parse(url.path.toString().stripSuffix(".git").stripSuffix("/"))
      path.parts match
        case Vector(org, repo, _*) =>
          Success(
            url
              .withScheme("https")
              .withPathParts(Seq(org, repo))
              .withFragment(None)
              .withUserInfo(None)
          )
        case _ =>
          Failure(new Error("Could not parse the GitHub URL"))
    else
      Failure(new Error("Domain was not github.com"))

  def gitHubUrl(s: String): Try[AbsoluteUrl] = AbsoluteUrl.parseTry(s).flatMap(gitHubUrl)

  def gitHubGitUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    gitHubUrl(url).map { gitHubUrl =>
      val newParts = Seq(gitHubUrl.path.parts.head, gitHubUrl.path.parts.last + ".git")
      gitHubUrl.withPathParts(newParts)
    }

  def gitHubIssuesUrl(url: AbsoluteUrl): Try[AbsoluteUrl] =
    gitHubUrl(url).map(_.addPathPart("issues"))

  def maybeGitHubOrg(url: AbsoluteUrl): Option[String] =
    url.path.parts.headOption

  def maybeGitHubRepo(url: AbsoluteUrl): Option[String] =
    url.path.parts match
      case _ +: repo +: _ => Some(repo)
      case _ => None
