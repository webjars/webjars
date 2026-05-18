package webjars.utils

import webjars.config.AppConfig
import webjars.utils.Deployable.Version
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

import scala.util.{Failure, Success, Try}

trait GitHub:
  def maybeAuthToken: Option[String]
  def currentUrls(url: URL): ZIO[Scope, Throwable, (URL, URL, URL)]
  def raw(gitHubUrl: URL, tagCommitOrBranch: String, fileName: String): ZIO[Scope, Throwable, String]
  def allPages[T](path: String, accumulator: Set[T] = Set.empty[T])(mapFunction: Response => ZIO[Scope, Throwable, Set[T]]): ZIO[Scope, Throwable, Set[T]]
  def tags(repo: String): ZIO[Scope, Throwable, Set[Version]]

case class GitHubLive(client: Client, config: AppConfig, cache: Cache) extends GitHub:

  lazy val maybeAuthToken: Option[String] = config.githubAuthToken

  private def wsRequest(path: String, accessToken: String): Request =
    Request.get(URL.unsafeParse(s"https://api.github.com/$path"))
      .addHeader(Header.Authorization.Bearer(accessToken))
      .addHeader(Header.Accept(MediaType.application.json))

  def currentUrls(url: URL): ZIO[Scope, Throwable, (URL, URL, URL)] =
    cache.get[(URL, URL, URL)](url.encode, 1.day) {
      def urls(location: String) =
        val newUrlsTry = for
          gitHubUrl <- GitHub.gitHubUrl(location)
          sourceConnectionUri <- GitHub.gitHubGitUrl(gitHubUrl)
          issuesUrl <- GitHub.gitHubIssuesUrl(gitHubUrl)
        yield (gitHubUrl, sourceConnectionUri, issuesUrl)
        ZIO.fromTry(newUrlsTry)

      val baseRequest = Request.get(url)
        .addHeader(Header.Custom("Follow-Redirects", "false"))
      val reqWithAuth = maybeAuthToken.fold(baseRequest)(token =>
        baseRequest.addHeader(Header.Authorization.Bearer(token))
      )

      defer:
        val response = client.batched(reqWithAuth).run
        response.status match
          case Status.MovedPermanently =>
            response.header(Header.Location) match
              case Some(location) =>
                URL.parseTry(location.url.encode) match
                  case Success(locationUrl) => currentUrls(locationUrl).run
                  case Failure(error) => ZIO.fail(error).run
              case None =>
                ZIO.fail(ServerError(s"GitHub said that ${url.encode} was moved but did not provide a new location", response.status.code)).run
          case Status.Ok =>
            urls(url.encode).run
          case _ =>
            ZIO.fail(ServerError(s"Could not get the current URL for ${url.encode} because status was ${response.status.code}", response.status.code)).run
    }

  def raw(gitHubUrl: URL, tagCommitOrBranch: String, fileName: String): ZIO[Scope, Throwable, String] =
    defer:
      val rawUrl = s"https://raw.githubusercontent.com${gitHubUrl.path.encode}/$tagCommitOrBranch/$fileName"
      val baseRequest = Request.get(URL.unsafeParse(rawUrl))
      val request = maybeAuthToken.fold(baseRequest)(token =>
        baseRequest.addHeader(Header.Authorization.Bearer(token))
      )
      val response = client.batched(request).run
      response.status match
        case Status.Ok => response.body.asString.run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(body)).run

  def allPages[T](path: String, accumulator: Set[T] = Set.empty[T])(mapFunction: Response => ZIO[Scope, Throwable, Set[T]]): ZIO[Scope, Throwable, Set[T]] =
    val request = maybeAuthToken.fold(
      Request.get(URL.unsafeParse(s"https://api.github.com/$path"))
    )(accessToken =>
      wsRequest(path, accessToken)
    )

    defer:
      val response = client.batched(request).run
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

  def gitHubUrl(url: URL): Try[URL] =
    if url.host.exists(_.endsWith("github.com")) then
      val rawPath = url.path.encode.stripSuffix(".git").stripSuffix("/")
      val parts = Path.decode(rawPath).segments
      parts.toVector match
        case Vector(org, repo, _*) =>
          Success(
            url
              .scheme(Scheme.HTTPS)
              .path(Path.root / org / repo)
              .copy(fragment = None)
          )
        case _ =>
          Failure(new Error("Could not parse the GitHub URL"))
    else
      Failure(new Error("Domain was not github.com"))

  def gitHubUrl(s: String): Try[URL] = URL.parseTry(s).flatMap(gitHubUrl)

  def gitHubGitUrl(url: URL): Try[URL] =
    gitHubUrl(url).map { gitHubUrl =>
      val segs = gitHubUrl.path.segments
      val newPath = Path.root / segs.head / (segs.last + ".git")
      gitHubUrl.path(newPath)
    }

  def gitHubIssuesUrl(url: URL): Try[URL] =
    gitHubUrl(url).map(_ / "issues")

  def maybeGitHubOrg(url: URL): Option[String] =
    url.path.segments.headOption

  def maybeGitHubRepo(url: URL): Option[String] =
    url.path.segments.toVector match
      case _ +: repo +: _ => Some(repo)
      case _ => None
