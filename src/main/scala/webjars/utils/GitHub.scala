package webjars.utils

import webjars.config.AppConfig
import webjars.utils.Deployable.Version
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

import java.net.URLEncoder as JdkUrlEncoder
import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}

/** Lightweight helper since we only need it at call sites that are
 *  building a GitHub query string. */
private object URLEncoder:
  def encode(s: String): String = JdkUrlEncoder.encode(s, StandardCharsets.UTF_8)

trait GitHub:
  def maybeAuthToken: Option[String]
  def currentUrls(url: URL): ZIO[Scope, Throwable, (URL, URL, URL)]
  def raw(gitHubUrl: URL, tagCommitOrBranch: String, fileName: String): ZIO[Scope, Throwable, String]
  def allPages[T](path: String, accumulator: Set[T] = Set.empty[T])(mapFunction: Response => ZIO[Scope, Throwable, Set[T]]): ZIO[Scope, Throwable, Set[T]]
  def tags(repo: String): ZIO[Scope, Throwable, Set[Version]]

  /** Find an open issue with the given exact title.
   *
   *  Used by the deploy-failure tracker as a safety net: when our Redis
   *  forward index has no entry for the GAV, we still want to comment on
   *  an existing tracking issue rather than file a fresh duplicate (e.g.
   *  if Redis was wiped or never persisted the prior file). Filters by
   *  the supplied label so we only consider issues we ourselves filed.
   *
   *  Pulls the first page (100 results) of open issues with the label and
   *  matches by title client-side. If we ever exceed 100 concurrently
   *  open `deploy-failure` issues we'll silently miss the older entries —
   *  that's a fine limit for now (it'd mean either a major outage or a
   *  classification-bug-spam loop, both of which deserve attention).
   *
   *  Authentication is required; without a token GitHub rate-limits us
   *  to 60 req/h per IP and the listing is empty/unauthenticated. */
  def findOpenIssueByTitle(repo: GitHub.Repo, title: String, label: String): ZIO[Scope, Throwable, Option[GitHub.Issue]]

  /** Open a new issue on `repo` with the given title, body, and labels.
   *  Returns the created issue's `(number, url)` pair. */
  def createIssue(repo: GitHub.Repo, title: String, body: String, labels: Set[String]): ZIO[Scope, Throwable, GitHub.Issue]

  /** Append a comment to an existing issue. */
  def commentOnIssue(repo: GitHub.Repo, issueNumber: Int, body: String): ZIO[Scope, Throwable, Unit]

  /** Close an issue, optionally with a final comment.
   *  Used by the deploy-failure tracker to auto-close on a successful
   *  later deploy of the same GAV. */
  def closeIssue(repo: GitHub.Repo, issueNumber: Int, comment: Option[String]): ZIO[Scope, Throwable, Unit]

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
          ZIO.fail(ServerError(body, response.status.code)).run

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

  /** Build an authenticated request for `https://api.github.com/<path>`,
   *  failing fast if no auth token is configured. The deploy-failure
   *  issue endpoints all require `Issues: write` — no point hitting them
   *  unauthenticated. */
  private def authedJsonRequest(path: String): ZIO[Any, Throwable, Request] =
    ZIO.fromOption(maybeAuthToken).orElseFail(
      UnauthorizedError("GitHub auth token (config webjars.github.auth-token / GITHUB_TOKEN) is required for issue operations")
    ).map { token =>
      Request.get(URL.unsafeParse(s"https://api.github.com/$path"))
        .addHeader(Header.Authorization.Bearer(token))
        .addHeader(Header.Accept(MediaType.application.json))
    }

  def findOpenIssueByTitle(repo: GitHub.Repo, title: String, label: String): ZIO[Scope, Throwable, Option[GitHub.Issue]] =
    import zio.json.ast.Json
    defer:
      val req = authedJsonRequest(
        s"repos/${repo.owner}/${repo.name}/issues?state=open&labels=${URLEncoder.encode(label)}&per_page=100"
      ).run
      val response = client.batched(req).run
      response.status match
        case Status.Ok =>
          val body = response.body.asString.run
          val issues = ZIO.fromEither(body.fromJson[List[Json]].left.map(new Exception(_))).run
          // Iterate, find the first issue whose title matches exactly. Use
          // `view` to keep this lazy in case the listing is large. We
          // intentionally do NOT match by GAV substring — caller-supplied
          // titles are deterministic, so an exact match is unambiguous.
          ZIO.succeed(
            issues.iterator
              .flatMap { json =>
                for
                  obj    <- json.asObject
                  num    <- obj.get("number").flatMap(_.asNumber).map(_.value.intValue)
                  t      <- obj.get("title").flatMap(_.asString)
                  urlStr <- obj.get("html_url").flatMap(_.asString)
                  url    <- URL.parseOption(urlStr)
                  if t == title
                yield GitHub.Issue(num, url)
              }
              .nextOption()
          ).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(ServerError(body, response.status.code)).run

  def createIssue(repo: GitHub.Repo, title: String, body: String, labels: Set[String]): ZIO[Scope, Throwable, GitHub.Issue] =
    import zio.json.ast.Json
    defer:
      val payload = Json.Obj(
        "title"  -> Json.Str(title),
        "body"   -> Json.Str(body),
        "labels" -> Json.Arr(Chunk.fromIterable(labels.map(Json.Str(_)))),
      ).toJson
      val req = authedJsonRequest(s"repos/${repo.owner}/${repo.name}/issues")
        .map(_.copy(method = Method.POST).withBody(Body.fromString(payload)).addHeader(Header.ContentType(MediaType.application.json))).run
      val response = client.batched(req).run
      response.status match
        case Status.Created =>
          val respBody = response.body.asString.run
          val json = ZIO.fromEither(respBody.fromJson[Json].left.map(new Exception(_))).run
          val maybeIssue = for
            obj    <- json.asObject
            num    <- obj.get("number").flatMap(_.asNumber).map(_.value.intValue)
            urlStr <- obj.get("html_url").flatMap(_.asString)
            url    <- URL.parseOption(urlStr)
          yield GitHub.Issue(num, url)
          ZIO.fromOption(maybeIssue).orElseFail(new Exception(s"Could not parse created-issue response: $respBody")).run
        case _ =>
          val errBody = response.body.asString.run
          ZIO.fail(ServerError(errBody, response.status.code)).run

  def commentOnIssue(repo: GitHub.Repo, issueNumber: Int, body: String): ZIO[Scope, Throwable, Unit] =
    import zio.json.ast.Json
    defer:
      val payload = Json.Obj("body" -> Json.Str(body)).toJson
      val req = authedJsonRequest(s"repos/${repo.owner}/${repo.name}/issues/$issueNumber/comments")
        .map(_.copy(method = Method.POST).withBody(Body.fromString(payload)).addHeader(Header.ContentType(MediaType.application.json))).run
      val response = client.batched(req).run
      response.status match
        case Status.Created => ()
        case _ =>
          val errBody = response.body.asString.run
          ZIO.fail(ServerError(errBody, response.status.code)).run

  def closeIssue(repo: GitHub.Repo, issueNumber: Int, comment: Option[String]): ZIO[Scope, Throwable, Unit] =
    import zio.json.ast.Json
    defer:
      // Comment first (if any) so the close event captures both in one
      // notification rather than two emails. Errors on commenting still
      // let the close attempt proceed.
      ZIO.foreachDiscard(comment)(c => commentOnIssue(repo, issueNumber, c).ignoreLogged).run
      val payload = Json.Obj("state" -> Json.Str("closed")).toJson
      val baseReq = authedJsonRequest(s"repos/${repo.owner}/${repo.name}/issues/$issueNumber").run
      val req = baseReq
        .copy(method = Method.PATCH)
        .withBody(Body.fromString(payload))
        .addHeader(Header.ContentType(MediaType.application.json))
      val response = client.batched(req).run
      response.status match
        case Status.Ok => ()
        case _ =>
          val errBody = response.body.asString.run
          ZIO.fail(ServerError(errBody, response.status.code)).run

object GitHub:

  /** A GitHub `<owner>/<repo>` pair. Convenience over passing two strings
   *  through the issue API. */
  case class Repo(owner: String, name: String):
    def slug: String = s"$owner/$name"
  object Repo:
    def parse(s: String): Option[Repo] = s.split('/') match
      case Array(o, n) if o.nonEmpty && n.nonEmpty => Some(Repo(o, n))
      case _                                       => None

  /** Minimal projection of the GitHub issue resource — the parts the
   *  deploy-failure tracker actually uses. */
  case class Issue(number: Int, htmlUrl: URL)

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
