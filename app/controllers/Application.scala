package controllers

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import models.WebJar
import org.joda.time._
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import play.api.Play
import play.api.Play.current
import play.api.libs.MimeTypes
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.data.Forms._
import play.api.data._
import play.api.mvc.{Action, Controller, Request, Result}
import utils.MavenCentral.{NotFoundResponseException, UnexpectedResponseException}
import utils.{GithubUtil, MavenCentral}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.hashing.MurmurHash3

object Application extends Controller {

  val X_GITHUB_ACCESS_TOKEN = "X-GITHUB-ACCESS-TOKEN"

  val github = GithubUtil(Play.current)

  def index = Action.async { implicit request =>
    MavenCentral.allWebJars.map { allWebJars =>
      val acceptHash = MurmurHash3.seqHash(request.acceptedTypes)
      val hash = MurmurHash3.listHash(allWebJars, acceptHash)
      val etag = "\"" + hash + "\""
      if (request.headers.get(IF_NONE_MATCH).contains(etag)) {
        NotModified
      }
      else {
        Ok(views.html.index(allWebJars)).withHeaders(ETAG -> etag)
      }
    } recover {
      case e: Exception =>
        InternalServerError(views.html.index(Seq.empty[WebJar]))
    }
  }

  def allWebJars = CorsAction {
    Action.async { implicit request =>
      MavenCentral.allWebJars.map { allWebJars =>
        val acceptHash = MurmurHash3.seqHash(request.acceptedTypes)
        val hash = MurmurHash3.listHash(allWebJars, acceptHash)
        val etag = "\"" + hash + "\""
        if (request.headers.get(IF_NONE_MATCH).contains(etag)) {
          NotModified
        }
        else {
          Ok(Json.toJson(allWebJars)).withHeaders(ETAG -> etag)
        }
      } recover {
        case e: Exception =>
          InternalServerError(Json.arr())
      }
    }
  }
  
  def listFiles(artifactId: String, version: String) = CorsAction {
    Action.async { implicit request =>
      MavenCentral.getFileList(artifactId, version).map { fileList =>
          render {
            case Accepts.Html() => Ok(views.html.filelist(artifactId, version, fileList))
            case Accepts.Json() => Ok(Json.toJson(fileList))
          }
      } recover {
        case nf: NotFoundResponseException =>
          NotFound(s"WebJar Not Found $artifactId : $version")
        case ure: UnexpectedResponseException =>
          Status(ure.response.status)(s"Problems retrieving WebJar ($artifactId : $version) - ${ure.response.statusText}")
      }
    }
  }

  // max 10 requests per minute
  lazy val fileRateLimiter = Akka.system.actorOf(Props(classOf[RequestTracker], 10, Period.minutes(1)))
  
  def file(artifactId: String, webJarVersion: String, file: String) = CorsAction {
    Action.async { request =>
      val pathPrefix = s"META-INF/resources/webjars/$artifactId/"

      MavenCentral.getFile(artifactId, webJarVersion).map { jarInputStream =>
        Stream.continually(jarInputStream.getNextJarEntry).takeWhile(_ != null).find { jarEntry =>
          // this allows for sloppyness where the webJarVersion and path differ
          // todo: eventually be more strict but since this has been allowed many WebJars do not have version and path consistency
          jarEntry.getName.startsWith(pathPrefix) && jarEntry.getName.endsWith(s"/$file")
        }.fold {
          jarInputStream.close()
          NotFound(s"Found WebJar ($artifactId : $webJarVersion) but could not find: $pathPrefix$webJarVersion/$file")
        } { jarEntry =>
          val enumerator = Enumerator.fromStream(jarInputStream)
          enumerator.onDoneEnumerating(jarInputStream.close())

          //// From Play's Assets controller
          val contentType = MimeTypes.forFileName(file).map(m => m + addCharsetIfNeeded(m)).getOrElse(BINARY)
          ////

          Ok.feed(enumerator).as(contentType).withHeaders(
            CACHE_CONTROL -> "max-age=290304000, public",
            DATE -> df.print({ new java.util.Date }.getTime),
            LAST_MODIFIED -> df.print(jarEntry.getLastModifiedTime.toMillis)
          )
        }
      } recover {
        case nf: NotFoundResponseException =>
          NotFound(s"WebJar Not Found $artifactId : $webJarVersion")
        case ure: UnexpectedResponseException =>
          Status(ure.response.status)(s"Problems retrieving WebJar ($artifactId : $webJarVersion) - ${ure.response.statusText}")
        case e: Exception =>
          InternalServerError(s"Could not find WebJar ($artifactId : $webJarVersion)\n${e.getMessage}")
      }
    }
  }

  def fileOptions(artifactId: String, version: String, file: String) = CorsAction {
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
        Ok(views.html.webJarRequest(weSome(accessToken), Some(login)))
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
