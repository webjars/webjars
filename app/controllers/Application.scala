package controllers

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import com.google.inject.ImplementedBy
import models.{WebJar, WebJarType}
import play.api.data.Forms._
import play.api.data._
import play.api.http.{ContentTypes, HttpEntity, MimeTypes}
import play.api.libs.EventSource
import play.api.libs.concurrent.Futures
import play.api.libs.json.Json
import play.api.mvc._
import play.api.{Environment, Logging, Mode}
import utils.MavenCentral.ExistingWebJarRequestException
import utils._

import java.io.FileNotFoundException
import java.net.URL
import java.util.concurrent.TimeoutException
import javax.inject.Inject
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure
import scala.util.hashing.MurmurHash3

class Application @Inject() (git: Git, gitHub: GitHub, cache: Cache, mavenCentral: MavenCentral, deployWebJar: DeployWebJar, webJarsFileService: WebJarsFileService, actorSystem: ActorSystem, environment: Environment, futures: Futures)
                            (classic: Classic, bower: Bower, npm: NPM, bowerGitHub: BowerGitHub)
                            (allView: views.html.all, indexView: views.html.index, webJarRequestView: views.html.webJarRequest, contributingView: views.html.contributing, documentationView: views.html.documentation)
                            (fetchConfig: FetchConfig)
                            (implicit ec: ExecutionContext) extends InjectedController with Logging {

  private val allWebJarTypes = Set(classic, bowerGitHub, bower, npm)

  private val X_GITHUB_ACCESS_TOKEN = "X-GITHUB-ACCESS-TOKEN"

  private[controllers] val MAX_POPULAR_WEBJARS = 20

  private val WEBJAR_FETCH_ERROR = """
    Looks like there was an error fetching the WebJars.
    Until the issue is resolved you can search on search.maven.org for <a href="https://search.maven.org/search?q=g:org.webjars">Classic WebJars</a> or <a href="https://search.maven.org/search?q=g:org.webjars.npm">NPM WebJars</a>.
    If this problem persists please <a href=\"https://github.com/webjars/webjars/issues/new\">file an issue</a>.
    """

  private def maybeCached[A](request: RequestHeader, f: Seq[A] => Result)(seq: Seq[A]): Result = {
    environment.mode match {
      case Mode.Dev =>
        f(seq)
      case _ =>
        val hash = MurmurHash3.seqHash(seq)
        val etag = "\"" + hash + "\""
        if (request.headers.get(IF_NONE_MATCH).contains(etag)) {
          NotModified
        }
        else {
          f(seq).withHeaders(ETAG -> etag)
        }
    }
  }

  private val allDeployables = Set(npm, bower, bowerGitHub)

  private def webJarsWithTimeout(maybeWebJarType: Option[WebJarType] = None): Future[List[WebJar]] = {
    val fetcher = maybeWebJarType.fold {
      cache.get[List[WebJar]]("webjars-all", 1.hour) {
        mavenCentral.webJarsSorted()
      }
    } { webJarType =>
      cache.get[List[WebJar]](s"webjars-$webJarType", 1.hour) {
        mavenCentral.webJarsSorted(Some(webJarType))
      }
    }

    val future = TimeoutFuture(fetchConfig.timeout)(fetcher)
    future.onComplete {
      case Failure(e: TimeoutException) => logger.debug("Timeout fetching WebJars", e)
      case Failure(e: MavenCentral.ExistingWebJarRequestException) => logger.debug("Existing WebJar Request", e)
      case Failure(e) => logger.error("Error loading WebJars", e)
      case _ => ()
    }
    future
  }

  private[controllers] def sortedMostPopularWebJars: Future[Seq[WebJar]] = {
    cache.get[Seq[WebJar]]("popular-webjars", 1.hour) {
      webJarsWithTimeout().map { allWebJars =>
        allWebJars.take(MAX_POPULAR_WEBJARS)
      }
    }
  }

  def index = Action.async { request =>
    sortedMostPopularWebJars.map(maybeCached(request, webJars => Ok(indexView(Left(webJars))))).recoverWith {
      case _: TimeoutException =>
        Future.successful(Redirect(routes.Application.index()))
      case _: ExistingWebJarRequestException =>
        futures.delay(fetchConfig.timeout).map(_ => Redirect(routes.Application.index()))
      case e: Exception =>
        logger.error("index WebJar fetch failed", e)
        Future.successful(InternalServerError(indexView(Right(WEBJAR_FETCH_ERROR))))
    }
  }

  def popularWebJars = Action.async { request =>
    sortedMostPopularWebJars.map(maybeCached(request, webJars => Ok(views.html.webJarList(Left(webJars))))).recover {
      case _: Exception =>
        InternalServerError(views.html.webJarList(Right(WEBJAR_FETCH_ERROR)))
    }
  }

  def searchWebJars(query: String, groupIds: List[String]) = Action.async { implicit request =>
    val queryLowerCase = query.toLowerCase.stripPrefix("org.webjars").stripPrefix("webjars")

    def filter(webJar: WebJar): Boolean = {
        webJar.name.toLowerCase.contains(queryLowerCase) ||
        webJar.groupId.toLowerCase.stripPrefix("org.webjars").stripPrefix("webjars").contains(queryLowerCase) ||
        webJar.artifactId.toLowerCase.contains(queryLowerCase)
    }

    val matchesFuture = Future.reduceLeft {
      groupIds.map { groupId =>
        webJarsWithTimeout(WebJarType.fromGroupId(groupId, allWebJarTypes)).map(_.filter(filter))
      }
    } (_ ++ _)

    matchesFuture.map { matchingWebJars =>
      render {
        case Accepts.Html() => Ok(views.html.webJarList(Left(matchingWebJars)))
        case Accepts.Json() => Ok(Json.toJson(matchingWebJars))
      }
    } recover {
      case e: Exception =>
        logger.error("searchWebJars failed", e)
        render {
          case Accepts.Html() => InternalServerError(views.html.webJarList(Right(WEBJAR_FETCH_ERROR)))
          case Accepts.Json() => InternalServerError(Json.toJson(Seq.empty[WebJar]))
        }
    }
  }

  def webJarList(groupId: String) = Action.async { implicit request =>
    webJarsWithTimeout(WebJarType.fromGroupId(groupId, allWebJarTypes)).map {
      maybeCached(request, webJars => Ok(Json.toJson(webJars)))
    } recover {
      case _: Exception =>
        InternalServerError(Json.toJson(Seq.empty[WebJar]))
    }
  }

  def classicList = Action {
    Redirect(routes.Application.index())
  }

  def bowerList = Action {
    Redirect(routes.Application.index())
  }

  def npmList = Action {
    Redirect(routes.Application.index())
  }

  def packageExists(webJarType: String, packageNameOrGitRepo: String) = Action.async {
    WebJarType.fromString(webJarType, allDeployables).fold {
      Future.successful(BadRequest(s"Specified WebJar type '$webJarType' can not be deployed"))
    } { deployable =>
      deployable.versions(packageNameOrGitRepo).map { _ =>
        Ok(<root></root>)
      } recover { case e: Exception =>
        InternalServerError(e.getMessage)
      }
    }
  }

  def packageVersions(webJarType: String, packageNameOrGitRepo: String, maybeBranch: Option[String]) = Action.async {
    WebJarType.fromString(webJarType, allDeployables).fold {
      Future.successful(BadRequest(s"Specified WebJar type '$webJarType' can not be deployed"))
    } { deployable =>

      val packageVersionsFuture = maybeBranch.fold {
        cache.get[Seq[String]](s"$webJarType-versions-$packageNameOrGitRepo", 1.hour) {
          deployable.versions(packageNameOrGitRepo).map(_.toSeq.sorted(VersionStringOrdering).reverse)
        }
      } { branch =>
        cache.get[Seq[String]](s"$webJarType-versions-$packageNameOrGitRepo-$branch", 1.hour) {
          git.versionsOnBranch(packageNameOrGitRepo, branch)
        }
      }

      packageVersionsFuture.map { versions =>
        Ok(Json.toJson(versions))
      } recover {
        case _: Exception =>
          InternalServerError
      }
    }
  }

  def allWebJars = CorsAction {
    Action.async { implicit request =>
      webJarsWithTimeout().map {
        maybeCached(request, { webJars =>
          render {
            case Accepts.Html() => Ok(allView(Left(webJars)))
            case Accepts.Json() => Ok(Json.toJson(webJars))
          }
        })
      } recover {
        case e: Exception =>
          logger.error("allWebJars fetch error", e)
          render {
            case Accepts.Html() => InternalServerError(allView(Right(WEBJAR_FETCH_ERROR)))
            case Accepts.Json() => InternalServerError(Json.arr())
          }
      }
    }
  }

  def listFiles(groupId: String, artifactId: String, version: String) = CorsAction {
    Action.async { implicit request =>
      webJarsFileService.getFileList(groupId, artifactId, version).map { fileList =>
        render {
          case Accepts.Html() => Ok(views.html.filelist(groupId, artifactId, version, fileList))
          case Accepts.Json() => Ok(Json.toJson(fileList))
        }
      } recover {
        case _: FileNotFoundException =>
          NotFound(s"WebJar Not Found $groupId : $artifactId : $version")
        case e: Exception =>
          InternalServerError(s"Problems retrieving WebJar ($groupId : $artifactId : $version) - ${e.getMessage}")
      }
    }
  }

  def file(groupId: String, artifactId: String, version: String, file: String) = Action {
    MovedPermanently(s"https://webjars-file-service.herokuapp.com/files/$groupId/$artifactId/$version/$file")
  }

  def fileOptions(file: String) = CorsAction {
    Action {
      Ok.withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> Seq(CONTENT_TYPE).mkString(","))
    }
  }

  def documentation = Action {
    Ok(documentationView())
  }

  def contributing = Action {
    Ok(contributingView())
  }

  def webJarRequest = Action.async { request =>
    request.flash.get(X_GITHUB_ACCESS_TOKEN).map { accessToken =>
      gitHub.user(accessToken).map { user =>
        val login = (user \ "login").as[String]
        Ok(webJarRequestView(Application.webJarRequestForm, Some(accessToken), Some(login)))
      }
    } getOrElse {
      Future.successful(Ok(webJarRequestView(Application.webJarRequestForm)))
    }
  }

  def makeWebJarRequest = Action.async(parse.formUrlEncoded) { implicit request =>

    Application.webJarRequestForm.bindFromRequest().fold(
      formWithErrors => {
        val gitHubToken = request.body.get("gitHubToken").flatMap(_.headOption)
        val gitHubUsername = request.body.get("gitHubUsername").flatMap(_.headOption)
        Future.successful(BadRequest(webJarRequestView(formWithErrors, gitHubToken, gitHubUsername)))
      },
      webJarRequest => {
        gitHub.user(webJarRequest.gitHubToken).flatMap { user =>
          val login = (user \ "login").asOpt[String].getOrElse("")
          val email = (user \ "email").asOpt[String].getOrElse("")
          val name = (user \ "name").asOpt[String].getOrElse("")

          gitHub.contents(webJarRequest.gitHubToken, "webjars", "webjars-template-zip", "pom.xml").flatMap { templatePom =>
            val pom = templatePom
              .replace("WEBJAR_ID", webJarRequest.id)
              .replace("UPSTREAM_VERSION", webJarRequest.version)
              .replace("WEBJAR_NAME", webJarRequest.name)
              .replace("UPSTREAM_ZIP_URL", webJarRequest.repoUrl + "/archive/v${version.unsnapshot}.zip")
              .replace("YOUR_ID", login)
              .replace("YOUR_NAME", name)
              .replace("YOUR_EMAIL", email)
              .replace("UPSTREAM_LICENSE_NAME", webJarRequest.licenseId)
              .replace("UPSTREAM_LICENSE_URL", webJarRequest.licenseUrl)
              .replace("MAIN_JS", webJarRequest.mainJs.map(_.stripSuffix(".js")).getOrElse(webJarRequest.id))

            val issueTitle = s"WebJar Request: ${webJarRequest.id}"

            val issueBody =
              s"""
                 |```
                 |$pom
                 |```
               """.stripMargin

            gitHub.createIssue(webJarRequest.gitHubToken, "webjars", "webjars", issueTitle, issueBody).map { issueResponse =>
              val url = (issueResponse \ "html_url").as[String]
              Redirect(url)
            }
          }
        }
      }
    )
  }

  def deploy(webJarType: String, nameOrUrlish: String, version: String): Action[AnyContent] = Action { implicit request =>
    WebJarType.fromString(webJarType, allDeployables).fold {
      BadRequest(s"Specified WebJar type '$webJarType' can not be deployed")
    } { deployable =>
      val source = deployWebJar.deploy(deployable, nameOrUrlish, version, true, false, false).recover {
        case e => e.getMessage
      } via {
        Flow[String].map(_ + "\n")
      } via {
        Flow[String].keepAlive(25.seconds, () => " ")
      }

      val acceptsText = Accepting(MimeTypes.TEXT)
      val acceptsEventStream = Accepting(MimeTypes.EVENT_STREAM)

      render {
        case acceptsText() =>
          Ok.chunked(source)
        case acceptsEventStream() =>
          Ok.chunked {
            source.via(EventSource.flow)
          } as ContentTypes.EVENT_STREAM
        case _ =>
          BadRequest("Could not determine content type")
      }

    }
  }

  def create(webJarType: String, nameOrUrlish: String, version: String) = Action.async { request =>

    val bodyAsJson = request.body.asJson
    val licenseOverride = bodyAsJson.flatMap { json =>
      (json \ "license").asOpt[Map[String, String]].map { licenses =>
        licenses.map[License] { case (name, url) =>
          LicenseWithNameAndUrl(name, new URL(url))
        }.toSet
      }
    }
    val groupIdOverride = bodyAsJson.flatMap { json =>
      (json \ "groupId").asOpt[String]
    }

    WebJarType.fromString(webJarType, allDeployables).fold {
      Future.successful(BadRequest(s"Specified WebJar type '$webJarType' can not be deployed"))
    } { deployable =>
      deployWebJar.create(deployable, nameOrUrlish, version, licenseOverride, groupIdOverride).map { case (name, bytes) =>
        val filename = name + ".jar"
        // taken from private method: play.api.mvc.Results.streamFile
        Result(
          ResponseHeader(
            OK,
            Map(CONTENT_DISPOSITION -> s"""attachment; filename="$filename"""")
          ),
          HttpEntity.Streamed(
            Source.single(ByteString(bytes)),
            Some(bytes.length.toLong),
            fileMimeTypes.forFileName(filename).orElse(Some(play.api.http.ContentTypes.BINARY))
          )
        )
      } recover {
        case e => BadRequest(e.getMessage)
      }
    }
  }

  def gitHubAuthorize = Action { implicit request =>
    Redirect(gitHub.authUrl())
  }

  def gitHubOauthCallback(code: String) = Action.async {
    gitHub.accessToken(code).map { accessToken =>
      Redirect(routes.Application.webJarRequest()).flashing(X_GITHUB_ACCESS_TOKEN -> accessToken)
    }
  }

  case class CorsAction[A](action: Action[A]) extends Action[A] {

    def apply(request: Request[A]): Future[Result] = {
      action(request).map(result => result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> "*"))
    }

    override def parser = action.parser
    override def executionContext = action.executionContext
  }

  def corsPreflight(file: String) = Action {
    Ok.withHeaders(
      ACCESS_CONTROL_ALLOW_ORIGIN -> "*",
      ACCESS_CONTROL_ALLOW_METHODS -> "GET"
    )
  }

  object TimeoutFuture {
    import java.util.concurrent.TimeoutException
    import scala.concurrent.Promise

    def apply[A](timeout: FiniteDuration)(future: Future[A]): Future[A] = {
      val promise = Promise[A]()

      actorSystem.scheduler.scheduleOnce(timeout) {
        promise.tryFailure(new TimeoutException(s"Future did not complete in $timeout"))
      }

      promise.completeWith(future)

      promise.future
    }
  }

}

@ImplementedBy(classOf[DefaultFetchConfig])
trait FetchConfig {
  val timeout: FiniteDuration
}

class DefaultFetchConfig extends FetchConfig {
  override val timeout: FiniteDuration = 25.seconds
}

object Application {

  case class WebJarRequest(gitHubToken: String, id: String, name: String, version: String, repoUrl: String, mainJs: Option[String], licenseId: String, licenseUrl: String)

  lazy val webJarRequestForm = Form(
    mapping(
      "gitHubToken" -> nonEmptyText,
      "webJarId" -> nonEmptyText,
      "webJarName" -> nonEmptyText,
      "webJarVersion" -> nonEmptyText,
      "repoUrl" -> nonEmptyText,
      "mainJs" -> optional(text),
      "licenseId" -> nonEmptyText,
      "licenseUrl" -> nonEmptyText
    )(WebJarRequest.apply)(WebJarRequest.unapply)
  )

}
