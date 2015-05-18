package controllers

import java.io.{BufferedInputStream, FileNotFoundException}
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import models.{WebJarCatalog, WebJar}
import org.joda.time._
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import play.api.Play
import play.api.Play.current
import play.api.cache.Cache
import play.api.libs.MimeTypes
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsValue, JsObject, JsArray, Json}
import play.api.data.Forms._
import play.api.data._
import play.api.libs.ws.WS
import play.api.mvc.Results.EmptyContent
import play.api.mvc._
import utils.MavenCentral.UnexpectedResponseException
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

  def index = Action {
    Ok(views.html.index())
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
      maybeCached(request, webJars => Ok(views.html.bowerList(webJars, pusher.key)))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.bowerList(Seq.empty[WebJar], pusher.key))
    }
  }

  def npmList = Action.async { request =>
    MavenCentral.webJars(WebJarCatalog.NPM).map {
      maybeCached(request, webJars => Ok(views.html.npmList(webJars, pusher.key)))
    } recover {
      case e: Exception =>
        InternalServerError(views.html.npmList(Seq.empty[WebJar], pusher.key))
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
        val versions = (json \ "versions" \\ "version").map(_.as[String])
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
      val classicFuture = MavenCentral.webJars(WebJarCatalog.CLASSIC)
      val bowerFuture = MavenCentral.webJars(WebJarCatalog.BOWER)

      val allFuture = for {
        classicWebJars <- classicFuture
        bowerWebJars <- bowerFuture
      } yield classicWebJars ++ bowerWebJars

      allFuture.map {
        maybeCached(request, webJars => Ok(Json.toJson(webJars)))
      } recover {
        case e: Exception =>
          InternalServerError(Json.arr())
      }
    }
  }
  
  def listFiles(groupId: String, artifactId: String, version: String) = CorsAction {
    Action.async { implicit request =>
      MavenCentral.getFileList(groupId, artifactId, version).map { fileList =>
          render {
            case Accepts.Html() => Ok(views.html.filelist(groupId, artifactId, version, fileList))
            case Accepts.Json() => Ok(Json.toJson(fileList))
          }
      } recover {
        case nf: FileNotFoundException =>
          NotFound(s"WebJar Not Found $groupId : $artifactId : $version")
        case ure: UnexpectedResponseException =>
          Status(ure.response.status)(s"Problems retrieving WebJar ($groupId : $artifactId : $version) - ${ure.response.statusText}")
        case e: Exception =>
          InternalServerError(e.getMessage)
      }
    }
  }

  def listFilesBower(artifactId: String, version: String) = CorsAction {
    Action.async { implicit request =>
      Future.successful(NotImplemented)
    }
  }

  // max 10 requests per minute
  lazy val fileRateLimiter = Akka.system.actorOf(Props(classOf[RequestTracker], 10, Period.minutes(1)))
  
  def file(groupId: String, artifactId: String, webJarVersion: String, file: String) = CorsAction {
    Action.async { request =>
      val pathPrefix = s"META-INF/resources/webjars/$artifactId/"

      Future.fromTry {
        MavenCentral.getFile(groupId, artifactId, webJarVersion).map { case (jarInputStream, inputStream) =>
          Stream.continually(jarInputStream.getNextJarEntry).takeWhile(_ != null).find { jarEntry =>
            // this allows for sloppyness where the webJarVersion and path differ
            // todo: eventually be more strict but since this has been allowed many WebJars do not have version and path consistency
            jarEntry.getName.startsWith(pathPrefix) && jarEntry.getName.endsWith(s"/$file")
          }.fold {
            jarInputStream.close()
            inputStream.close()
            NotFound(s"Found WebJar ($groupId : $artifactId : $webJarVersion) but could not find: $pathPrefix$webJarVersion/$file")
          } { jarEntry =>
            val bis = new BufferedInputStream(jarInputStream)
            val bArray = Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray
            bis.close()
            jarInputStream.close()
            inputStream.close()

            //// From Play's Assets controller
            val contentType = MimeTypes.forFileName(file).map(m => m + addCharsetIfNeeded(m)).getOrElse(BINARY)
            ////

            Ok(bArray).as(contentType).withHeaders(
              CACHE_CONTROL -> "max-age=290304000, public",
              DATE -> df.print((new java.util.Date).getTime),
              LAST_MODIFIED -> df.print(jarEntry.getLastModifiedTime.toMillis)
            )
          }
        }
      } recover {
        case nf: FileNotFoundException =>
          NotFound(s"WebJar Not Found $groupId : $artifactId : $webJarVersion")
        case ure: UnexpectedResponseException =>
          Status(ure.response.status)(s"Problems retrieving WebJar ($groupId : $artifactId : $webJarVersion) - ${ure.response.statusText}")
        case e: Exception =>
          InternalServerError(s"Could not find WebJar ($groupId : $artifactId : $webJarVersion)\n${e.getMessage}")
      }
    }
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
      val cmd = s"pub-bower $artifactId $version $channelId"
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
      val cmd = s"pub-npm $artifactId $version $channelId"
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

  case class RateLimit[A](actorRef: ActorRef)(action: Action[A]) extends Action[A] {

    implicit val actorTimeout = Timeout(1.second)

    def apply(request: Request[A]): Future[Result] = {
      // get the IP which might be in the form 1.2.3.4,6.7.8.9 in which case we want the last one
      val ip: IP = request.headers.get(X_FORWARDED_FOR).getOrElse(request.remoteAddress).split(",").reverse.head
      (actorRef ? ip).flatMap {
        case ExceededLimit => Future.successful(TooManyRequest)
        case _ => action(request)
      } recoverWith {
        case _ => action(request)
      }
    }

    lazy val parser = action.parser
  }

  type IP = String
  case object ExceededLimit
  case object UnderLimit

  class RequestTracker(maxRequests: Int, period: ReadablePeriod) extends Actor {

    type Requests = Seq[DateTime]

    var ipRates: Map[IP, Requests] = Map.empty[IP, Requests]

    override def receive = {
      case ip: IP =>
        val now = DateTime.now()

        // update the map
        ipRates.get(ip).fold {
          ipRates = ipRates + (ip -> Seq(now))
        } { requests =>
          val updatedRequests = requests.filter(_.isAfter(now.minus(period))) :+ now
          ipRates = ipRates.updated(ip, updatedRequests)
        }

        // determine if the rate has been exceeded
        if (ipRates(ip).size >= maxRequests) {
          sender ! ExceededLimit
        }
        else {
          sender ! UnderLimit
        }
    }

  }

  //// From Play's Asset controller

  private val timeZoneCode = "GMT"

  //Dateformatter is immutable and threadsafe
  private val df: DateTimeFormatter =
    DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss '" + timeZoneCode + "'").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))

  //Dateformatter is immutable and threadsafe
  private val dfp: DateTimeFormatter =
    DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))

  private lazy val defaultCharSet = Play.configuration.getString("default.charset").getOrElse("utf-8")

  private def addCharsetIfNeeded(mimeType: String): String =
    if (MimeTypes.isText(mimeType))
      "; charset=" + defaultCharSet
    else ""

  ////

}
