package controllers

import java.io.FileNotFoundException
import java.util.concurrent.TimeoutException
import javax.inject.Inject

import akka.actor.ActorSystem
import models.WebJarCatalog.WebJarCatalog
import models.{WebJar, WebJarCatalog}
import org.joda.time.DateTime
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json
import play.api.mvc._
import play.api.{Configuration, Environment, Logger, Mode}
import utils._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.hashing.MurmurHash3

class Application @Inject()(gitHub: GitHub, bower: Bower, npm: NPM, heroku: Heroku, pusher: Pusher, cache: Cache, mavenCentral: MavenCentral, npmWebJar: NPMWebJar, bowerWebJar: BowerWebJar, webJarsFileService: WebJarsFileService, actorSystem: ActorSystem, configuration: Configuration, environment: Environment)(mainView: views.html.main, allView: views.html.all, indexView: views.html.index, classicListView: views.html.classicList, npmbowerListView: views.html.npmbowerList, webJarRequestView: views.html.webJarRequest, contributingView: views.html.contributing, documentationView: views.html.documentation)(implicit ec: ExecutionContext) extends Controller {

  private val X_GITHUB_ACCESS_TOKEN = "X-GITHUB-ACCESS-TOKEN"

  private val MAX_POPULAR_WEBJARS = 20

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

  private val defaultTimeout = 25.seconds

  private def webJarsWithTimeout(maybeCatalog: Option[WebJarCatalog.WebJarCatalog] = None): Future[List[WebJar]] = {
    val fetcher = maybeCatalog.fold(mavenCentral.webJars)(mavenCentral.webJars)
    val future = TimeoutFuture(defaultTimeout)(fetcher)
    future.onFailure {
      case te: TimeoutException => Logger.debug("Timeout fetching WebJars", te)
      case e: Exception => Logger.error("Error loading WebJars", e)
    }
    future
  }

  private def sortedWebJars(counts: Seq[(WebJarCatalog, String, Int)], webJars: Seq[WebJar]): Seq[WebJar] = {
    counts.flatMap {
      case (webJarCatalog, artifactId, _) =>
        val maybeWebJar = webJars.find { webJar => (webJar.groupId == webJarCatalog.toString) && (webJar.artifactId == artifactId) }
        maybeWebJar.toSeq
    }
  }

  private def sortedMostPopularWebJars: Future[Seq[WebJar]] = {
    webJarsWithTimeout().flatMap { allWebJars =>
      val mostDownloadedFuture = cache.get[Seq[(WebJarCatalog, String, Int)]]("mostDownloaded", 1.day) {
        val lastMonth = DateTime.now().minusMonths(1)
        mavenCentral.mostDownloaded(lastMonth, MAX_POPULAR_WEBJARS).recoverWith {
          case e: Exception => mavenCentral.mostDownloaded(lastMonth.minusMonths(1), MAX_POPULAR_WEBJARS)
        }
      }

      mostDownloadedFuture.map { mostDownloaded =>
        sortedWebJars(mostDownloaded, allWebJars)
      }
    }
  }

  def index = Action.async { request =>
    sortedMostPopularWebJars.map(maybeCached(request, webJars => Ok(indexView(webJars)))).recover {
      case e: Exception =>
        InternalServerError(indexView(Seq.empty[WebJar]))
    }
  }

  def popularWebJars = Action.async { request =>
    sortedMostPopularWebJars.map(maybeCached(request, webJars => Ok(views.html.webJarList(webJars)))).recover {
      case e: Exception =>
        InternalServerError(views.html.webJarList(Seq.empty[WebJar], false))
    }
  }

  def searchWebJars(query: String) = Action.async { implicit request =>
    webJarsWithTimeout().flatMap { allWebJars =>

      val webJarStatsFuture = cache.get[Seq[(WebJarCatalog, String, Int)]]("stats", 1.day) {
        val lastMonth = DateTime.now().minusMonths(1)
        mavenCentral.getStats(lastMonth).recoverWith {
          case e: Exception => mavenCentral.getStats(lastMonth.minusMonths(1))
        }
      }

      webJarStatsFuture.map { webJarStats =>
        val matchingWebJars = allWebJars.filter { webJar =>
          webJar.name.toLowerCase.contains(query.toLowerCase) || webJar.artifactId.toLowerCase.contains(query.toLowerCase)
        }

        val sortedMatchingWebJars = sortedWebJars(webJarStats, matchingWebJars)

        render {
          case Accepts.Html() => Ok(views.html.webJarList(sortedMatchingWebJars))
          case Accepts.Json() => Ok(Json.toJson(sortedMatchingWebJars))
        }
      }
    } recover {
      case e: Exception =>
        render {
          case Accepts.Html() => InternalServerError(views.html.webJarList(Seq.empty[WebJar]))
          case Accepts.Json() => InternalServerError(Json.toJson(Seq.empty[WebJar]))
        }
    }
  }

  def classicList = Action.async { request =>
    webJarsWithTimeout(Some(WebJarCatalog.CLASSIC)).map {
      maybeCached(request, webJars => Ok(classicListView(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(classicListView(Seq.empty[WebJar]))
    }
  }

  def webJarList(groupId: String) = Action.async { implicit request =>
    val catalog = WebJarCatalog.withName(groupId)
    webJarsWithTimeout(Some(catalog)).map {
      maybeCached(request, webJars => Ok(Json.toJson(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(Json.toJson(Seq.empty[WebJar]))
    }
  }

  def bowerList = Action.async { request =>
    webJarsWithTimeout(Some(WebJarCatalog.BOWER)).map {
      maybeCached(request, webJars => Ok(npmbowerListView(webJars, pusher.maybeKey, "Bower", "bower")))
    } recover {
      case e: Exception =>
        InternalServerError(npmbowerListView(Seq.empty[WebJar], pusher.maybeKey, "Bower", "bower"))
    }
  }

  def npmList = Action.async { request =>
    webJarsWithTimeout(Some(WebJarCatalog.NPM)).map {
      maybeCached(request, webJars => Ok(npmbowerListView(webJars, pusher.maybeKey, "NPM", "npm")))
    } recover {
      case e: Exception =>
        InternalServerError(npmbowerListView(Seq.empty[WebJar], pusher.maybeKey, "NPM", "npm"))
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
        maybeCached(request, { webJars =>
          render {
            case Accepts.Html() => Ok(allView(webJars))
            case Accepts.Json() => Ok(Json.toJson(webJars))
          }
        })
      } recover {
        case e: Exception =>
          render {
            case Accepts.Html() => InternalServerError(allView(Seq.empty[WebJar]))
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

  def makeWebJarRequest = Action.async(parse.urlFormEncoded) { implicit request =>

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

  def deployBower(artifactId: String, version: String, channelId: String) = Action.async {
    val app = configuration.getString("bower.herokuapp").get
    val fork = configuration.getBoolean("bower.fork").get

    if (fork) {
      val cmd = s"pubbower $artifactId $version $channelId"
      heroku.dynoCreate(app, false, cmd, "Standard-2X").map { createJson =>
        Ok(createJson)
      }
    }
    else {
      bowerWebJar.release(artifactId, version, Some(channelId)).map { result =>
        Ok(Json.toJson(result))
      } recover {
        case e: Exception => InternalServerError(e.getMessage)
      }
    }
  }

  def deployNPM(nameOrUrlish: String, version: String, channelId: String) = Action.async {
    val app = configuration.getString("bower.herokuapp").get
    val fork = configuration.getBoolean("bower.fork").get

    if (fork) {
      val cmd = s"pubnpm $nameOrUrlish $version $channelId"
      heroku.dynoCreate(app, false, cmd, "Standard-2X").map { createJson =>
        Ok(createJson)
      }
    }
    else {
      npmWebJar.release(nameOrUrlish, version, Some(channelId)).map { result =>
        Ok(Json.toJson(result))
      } recover {
        case e: Exception => InternalServerError(e.getMessage)
      }
    }
  }

  def gitHubAuthorize = Action { implicit request =>
    Redirect(gitHub.authUrl)
  }

  def gitHubOauthCallback(code: String) = Action.async { implicit request =>
    gitHub.accessToken(code).map { accessToken =>
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
    import java.util.concurrent.TimeoutException

    import scala.concurrent.Promise

    def apply[A](timeout: FiniteDuration)(future: Future[A]): Future[A] = {
      val promise = Promise[A]()

      actorSystem.scheduler.scheduleOnce(timeout) {
        promise.tryFailure(new TimeoutException)
      }

      promise.completeWith(future)

      promise.future
    }
  }

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
