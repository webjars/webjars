package controllers

import java.io.FileNotFoundException
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import models.{WebJarCatalog, WebJar}
import org.joda.time._
import play.api.Play
import play.api.Play.current
import play.api.cache.Cache
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsObject, JsArray, Json}
import play.api.data.Forms._
import play.api.data._
import play.api.libs.ws.WS
import play.api.mvc._
import utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.hashing.MurmurHash3

object Application extends Controller {

  val X_GITHUB_ACCESS_TOKEN = "X-GITHUB-ACCESS-TOKEN"

  lazy val github = GithubUtil(Play.current)
  lazy val bower = Bower(ExecutionContext.global, WS.client(Play.current))
  lazy val npm = NPM(ExecutionContext.global, WS.client(Play.current))
  lazy val heroku = Heroku(ExecutionContext.global, WS.client(Play.current), Play.current.configuration)
  lazy val pusher = Pusher(ExecutionContext.global, WS.client(Play.current), Play.current.configuration)

  def index = Action.async { request =>
    MavenCentral.webJars.map {
      maybeCached(request, webJars => Ok(views.html.index(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.index(Seq.empty[WebJar]))
    }
  }

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

  def classicList = Action.async { request =>
    MavenCentral.webJars(WebJarCatalog.CLASSIC).map {
      maybeCached(request, webJars => Ok(views.html.classicList(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.classicList(Seq.empty[WebJar]))
    }
  }

  def webJarList(groupId: String) = Action.async { implicit request =>
    val catalog = WebJarCatalog.withName(groupId)
    MavenCentral.webJars(catalog).map {
      maybeCached(request, webJars => Ok(Json.toJson(webJars)))
    } recover {
      case e: Exception =>
        InternalServerError(Json.toJson(Seq.empty[WebJar]))
    }
  }

  def bowerList = Action.async { request =>
    MavenCentral.webJars(WebJarCatalog.BOWER).map {
      maybeCached(request, webJars => Ok(views.html.npmbowerList(webJars, pusher.key, "Bower", "bower")))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.npmbowerList(Seq.empty[WebJar], pusher.key, "Bower", "bower"))
    }
  }

  def npmList = Action.async { request =>
    MavenCentral.webJars(WebJarCatalog.NPM).map {
      maybeCached(request, webJars => Ok(views.html.npmbowerList(webJars, pusher.key, "NPM", "npm")))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.npmbowerList(Seq.empty[WebJar], pusher.key, "NPM", "npm"))
    }
  }

  def bowerPackageExists(packageName: String) = Action.async {
    bower.info(packageName).map(_ => Ok).recover { case e: Exception =>
      InternalServerError(e.getMessage)
    }
  }

  def bowerPackages(query: String, page: Int) = Action.async { request =>
    val pageSize = 30

    val allBowerPackagesFuture = Cache.getAs[JsArray]("bower-packages").fold {
      bower.all.map { json =>
        Cache.set("bower-packages", json, 1.hour)
        json
      }
    } (Future.successful)

    allBowerPackagesFuture.map { allBowerPackages =>

      // match on names, keywords, or description
      val allMatches = allBowerPackages.as[Seq[JsObject]].filter { bowerPackage =>
        (bowerPackage \ "name").asOpt[String].map(_.toLowerCase).exists(_.contains(query.toLowerCase)) ||
        (bowerPackage \ "website").asOpt[String].map(_.toLowerCase).exists(_.contains(query.toLowerCase)) ||
        (bowerPackage \ "keywords").asOpt[Seq[String]].getOrElse(Seq.empty[String]).exists(_.toLowerCase.contains(query.toLowerCase)) ||
        (bowerPackage \ "description").asOpt[String].map(_.toLowerCase).exists(_.contains(query.toLowerCase))
      }

      // sort by popularity
      val sorted = allMatches.sortBy(p => (p \ "stars").as[Int]).reverse

      // return this page
      val startIndex = (page - 1) * pageSize
      val results = sorted.slice(startIndex, startIndex + pageSize)

      val json = Json.obj(
        "results" -> Json.toJson(results),
        "total_count" -> sorted.size
      )

      Ok(json)
    } recover {
      case e: Exception =>
        InternalServerError(e.getMessage)
    }
  }

  def bowerPackageVersions(packageName: String) = Action.async { request =>
    val packageVersionsFuture = Cache.getAs[Seq[String]](s"bower-versions-$packageName").fold {
      bower.info(packageName).map { json =>
        val versions = (json \ "versions").as[Seq[String]]
        val cleanVersions = versions.filterNot(_.contains("sha"))
        Cache.set(s"bower-versions-$packageName", cleanVersions, 1.hour)
        cleanVersions
      }
    } (Future.successful)

    packageVersionsFuture.map { json =>
      Ok(Json.toJson(json))
    } recover {
      case e: Exception =>
        InternalServerError
    }
  }

  def npmPackageExists(packageName: String) = Action.async {
    npm.latest(packageName).map(_ => Ok).recover { case e: Exception =>
      InternalServerError(e.getMessage)
    }
  }

  def npmPackageVersions(packageName: String) = Action.async {
    val packageVersionsFuture = Cache.getAs[Seq[String]](s"npm-versions-$packageName").fold {
      npm.info(packageName).map { json =>
        val versions = (json \ "versions" \\ "version").map(_.as[String]).sorted(VersionOrdering).reverse
        Cache.set(s"npm-versions-$packageName", versions, 1.hour)
        versions
      }
    } (Future.successful)

    packageVersionsFuture.map { json =>
      Ok(Json.toJson(json))
    } recover {
      case e: Exception =>
        InternalServerError
    }
  }

  def allWebJars = CorsAction {
    Action.async { implicit request =>
      MavenCentral.webJars.map {
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
      BowerWebJar.release(artifactId, version, Some(channelId))(ExecutionContext.global, Play.current.configuration).map { result =>
        Ok(Json.toJson(result))
      } recover {
        case e: Exception => InternalServerError(e.getMessage)
      }
    }
  }

  def deployNPM(artifactId: String, version: String, channelId: String) = Action.async {
    val app = Play.current.configuration.getString("bower.herokuapp").get
    val fork = Play.current.configuration.getBoolean("bower.fork").get

    if (fork) {
      val cmd = s"pubnpm $artifactId $version $channelId"
      heroku.dynoCreate(app, false, cmd, "Standard-2X").map { createJson =>
        Ok(createJson)
      }
    }
    else {
      NPMWebJar.release(artifactId, version, Some(channelId))(ExecutionContext.global, Play.current.configuration).map { result =>
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

}
