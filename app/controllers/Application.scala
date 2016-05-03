package controllers

import java.io.FileNotFoundException

import models.{WebJar, WebJarCatalog}
import play.api.Play
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc._
import utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.hashing.MurmurHash3

object Application extends Controller {

  val X_GITHUB_ACCESS_TOKEN = "X-GITHUB-ACCESS-TOKEN"

  lazy val github = GithubUtil(Play.current)
  lazy val bower = Bower(ExecutionContext.global, WS.client(Play.current))
  lazy val npm = NPM(ExecutionContext.global, WS.client(Play.current))
  lazy val heroku = Heroku(ExecutionContext.global, WS.client(Play.current), Play.current.configuration)
  lazy val pusher = Pusher(ExecutionContext.global, WS.client(Play.current), Play.current.configuration)
  lazy val cache = Cache(ExecutionContext.global, Play.current)

  private def maybeCached[A](request: RequestHeader, f: Seq[A] => Result)(seq: Seq[A]): Result = {
    val hash = MurmurHash3.seqHash(seq)
    val etag = "\"" + hash + "\""
    if (request.headers.get(IF_NONE_MATCH).contains(etag)) {
      NotModified
    }
    else {
      f(seq).withHeaders(ETAG -> etag)
    }
  }

  val defaultTimeout = 25.seconds

  private def webJarsWithTimeout(): Future[List[WebJar]] = {
    TimeoutFuture(defaultTimeout)(MavenCentral.webJars)
  }

  private def webJarsWithTimeout(catalog: WebJarCatalog.WebJarCatalog): Future[List[WebJar]] = {
    TimeoutFuture(defaultTimeout)(MavenCentral.webJars(catalog))
  }

  def index = Action.async { request =>
    webJarsWithTimeout().map {
      maybeCached(request, webJars => Ok(views.html.index(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.index(Seq.empty[WebJar]))
    }
  }

  def classicList = Action.async { request =>
    webJarsWithTimeout(WebJarCatalog.CLASSIC).map {
      maybeCached(request, webJars => Ok(views.html.classicList(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.classicList(Seq.empty[WebJar]))
    }
  }

  def webJarList(groupId: String) = Action.async { implicit request =>
    val catalog = WebJarCatalog.withName(groupId)
    webJarsWithTimeout(catalog).map {
      maybeCached(request, webJars => Ok(Json.toJson(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(Json.toJson(Seq.empty[WebJar]))
    }
  }

  def bowerList = Action.async { request =>
    webJarsWithTimeout(WebJarCatalog.BOWER).map {
      maybeCached(request, webJars => Ok(views.html.npmbowerList(webJars, pusher.maybeKey, "Bower", "bower")))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.npmbowerList(Seq.empty[WebJar], pusher.maybeKey, "Bower", "bower"))
    }
  }

  def npmList = Action.async { request =>
    webJarsWithTimeout(WebJarCatalog.NPM).map {
      maybeCached(request, webJars => Ok(views.html.npmbowerList(webJars, pusher.maybeKey, "NPM", "npm")))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.npmbowerList(Seq.empty[WebJar], pusher.maybeKey, "NPM", "npm"))
    }
  }

  def bowerPackageExists(packageNameOrGitRepo: String) = Action.async {
    bower.versions(packageNameOrGitRepo).map(_ => Ok).recover {
      case e: Exception =>
        InternalServerError(e.getMessage)
    }
  }

  def bowerPackageVersions(packageNameOrGitRepo: String, maybeBranch: Option[String]) = Action.async { request =>

    val packageVersionsFuture = maybeBranch.fold {
      cache.get[Seq[String]](s"bower-versions-$packageNameOrGitRepo", 1.hour) {
        bower.versions(packageNameOrGitRepo)
      }
    } { branch =>
      cache.get[Seq[String]](s"bower-versions-$packageNameOrGitRepo-$branch", 1.hour) {
        bower.versionsOnBranch(packageNameOrGitRepo, branch)
      }
    }

    packageVersionsFuture.map { json =>
      Ok(Json.toJson(json))
    } recover {
      case e: Exception =>
        InternalServerError
    }
  }

  def npmPackageExists(packageNameOrGitRepo: String) = Action.async {
    npm.versions(packageNameOrGitRepo).map(_ => Ok).recover { case e: Exception =>
      InternalServerError(e.getMessage)
    }
  }

  def npmPackageVersions(packageNameOrGitRepo: String, maybeBranch: Option[String]) = Action.async {
    val packageVersionsFuture = maybeBranch.fold {
      cache.get[Seq[String]](s"npm-versions-$packageNameOrGitRepo", 1.hour) {
        npm.versions(packageNameOrGitRepo)
      }
    } { branch =>
      cache.get[Seq[String]](s"npm-versions-$packageNameOrGitRepo-$branch", 1.hour) {
        npm.versionsOnBranch(packageNameOrGitRepo, branch)
      }
    }

    packageVersionsFuture.map { versions =>
      Ok(Json.toJson(versions))
    } recover {
      case e: Exception =>
        InternalServerError
    }
  }

  def allWebJars = CorsAction {
    Action.async { implicit request =>
      webJarsWithTimeout().map {
        maybeCached(request, webJars => Ok(Json.toJson(webJars)))
      } recover {
        case e: Exception =>
          InternalServerError(Json.arr())
      }
    }
  }

  def listFiles(groupId: String, artifactId: String, version: String) = CorsAction {
    Action.async { implicit request =>
      WebJarsFileService.getFileList(groupId, artifactId, version).map { fileList =>
        render {
          case Accepts.Html() => Ok(views.html.filelist(groupId, artifactId, version, fileList))
          case Accepts.Json() => Ok(Json.toJson(fileList))
        }
      } recover {
        case nf: FileNotFoundException =>
          NotFound(s"WebJar Not Found $groupId : $artifactId : $version")
        case e: Exception =>
          InternalServerError(s"Problems retrieving WebJar ($groupId : $artifactId : $version) - ${e.getMessage}")
      }
    }
  }

  def file(groupId: String, artifactId: String, version: String, file: String) = Action {
    MovedPermanently(s"http://webjars-file-service.herokuapp.com/files/$groupId/$artifactId/$version/$file")
  }

  def fileOptions(file: String) = CorsAction {
    Action { request =>
      Ok.withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> Seq(CONTENT_TYPE).mkString(","))
    }
  }

  def documentation = Action {
    Ok(views.html.documentation())
  }

  def contributing = Action {
    Ok(views.html.contributing())
  }

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

  def webJarRequest = Action.async { request =>
    request.flash.get(X_GITHUB_ACCESS_TOKEN).map { accessToken =>
      github.user(accessToken).map { user =>
        val login = (user \ "login").as[String]
        Ok(views.html.webJarRequest(webJarRequestForm, Some(accessToken), Some(login)))
      }
    } getOrElse {
      Future.successful(Ok(views.html.webJarRequest(webJarRequestForm)))
    }
  }

  def makeWebJarRequest = Action.async(parse.urlFormEncoded) { implicit request =>

    webJarRequestForm.bindFromRequest().fold(
      formWithErrors => {
        val gitHubToken = request.body.get("gitHubToken").flatMap(_.headOption)
        val gitHubUsername = request.body.get("gitHubUsername").flatMap(_.headOption)
        Future.successful(BadRequest(views.html.webJarRequest(formWithErrors, gitHubToken, gitHubUsername)))
      },
      webJarRequest => {
        github.user(webJarRequest.gitHubToken).flatMap { user =>
          val login = (user \ "login").asOpt[String].getOrElse("")
          val email = (user \ "email").asOpt[String].getOrElse("")
          val name = (user \ "name").asOpt[String].getOrElse("")

          github.contents(webJarRequest.gitHubToken, "webjars", "webjars-template-zip", "pom.xml").flatMap { templatePom =>
            val pom = templatePom
              .replace("WEBJAR_ID", webJarRequest.id)
              .replace("UPSTREAM_VERSION", webJarRequest.version)
              .replace("WEBJAR_NAME", webJarRequest.name)
              .replace("UPSTREAM_ZIP_URL", webJarRequest.repoUrl + "/archive/v${upstream.version}.zip")
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

            github.createIssue(webJarRequest.gitHubToken, "webjars", "webjars", issueTitle, issueBody).map { issueResponse =>
              val url = (issueResponse \ "html_url").as[String]
              Redirect(url)
            }
          }
        }
      }
    )
  }

  def deployBower(artifactId: String, version: String, channelId: String) = Action.async {
    val app = Play.current.configuration.getString("bower.herokuapp").get
    val fork = Play.current.configuration.getBoolean("bower.fork").get

    if (fork) {
      val cmd = s"pubbower $artifactId $version $channelId"
      heroku.dynoCreate(app, false, cmd, "Standard-2X").map { createJson =>
        Ok(createJson)
      }
    }
    else {
      BowerWebJar.release(artifactId, version, Some(channelId))(ExecutionContext.global, Play.current.configuration, Play.current).map { result =>
        Ok(Json.toJson(result))
      } recover {
        case e: Exception => InternalServerError(e.getMessage)
      }
    }
  }

  def deployNPM(nameOrUrlish: String, version: String, channelId: String) = Action.async {
    val app = Play.current.configuration.getString("bower.herokuapp").get
    val fork = Play.current.configuration.getBoolean("bower.fork").get

    if (fork) {
      val cmd = s"pubnpm $nameOrUrlish $version $channelId"
      heroku.dynoCreate(app, false, cmd, "Standard-2X").map { createJson =>
        Ok(createJson)
      }
    }
    else {
      NPMWebJar.release(nameOrUrlish, version, Some(channelId))(ExecutionContext.global, Play.current.configuration, Play.current).map { result =>
        Ok(Json.toJson(result))
      } recover {
        case e: Exception => InternalServerError(e.getMessage)
      }
    }
  }

  def githubAuthorize  = Action { implicit request =>
    Redirect(github.authUrl)
  }

  def githubOauthCallback(code: String) = Action.async { implicit request =>
    github.accessToken(code).map { accessToken =>
      Redirect(routes.Application.webJarRequest()).flashing(X_GITHUB_ACCESS_TOKEN -> accessToken)
    }
  }

  case class CorsAction[A](action: Action[A]) extends Action[A] {

    def apply(request: Request[A]): Future[Result] = {
      action(request).map(result => result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> "*"))
    }

    lazy val parser = action.parser
  }

  def corsPreflight(path: String) = Action {
    Ok.withHeaders(
      ACCESS_CONTROL_ALLOW_ORIGIN -> "*",
      ACCESS_CONTROL_ALLOW_METHODS -> "GET"
    )
  }

  object TimeoutFuture {
    import play.api.libs.concurrent.{Promise => PlayPromise}
    import scala.concurrent.{Future, Promise}
    import java.util.concurrent.TimeoutException

    def apply[A](timeout: FiniteDuration)(future: Future[A]): Future[A] = {
      val promise = Promise[A]()

      PlayPromise.timeout(Unit, timeout).foreach { _ =>
        promise.tryFailure(new TimeoutException)
      }

      promise.completeWith(future)

      promise.future
    }
  }

}