package controllers

import controllers.WebJarAssets.locate
import controllers.routes.{StaticWebJarAssets => AssetRoutes}
import play.api.Play
import play.api.Play.current
import play.api.mvc.Controller

object StaticWebJarAssets extends Controller {

  def at(file: String) = Assets.at("/META-INF/resources/webjars", file)
  def atBower(file: String) = Assets.at("/META-INF/resources/webjars", file)

  lazy val maybeContentUrl = Play.configuration.getString("contentUrl")

  def getUrl(file: String) = {
    val atUrl = AssetRoutes.at(locate(file)).url
    maybeContentUrl.fold(atUrl)(_ + atUrl)
  }

  def getUrlBower(file: String) = {
    val atUrl = AssetRoutes.atBower(locate(file)).url
    maybeContentUrl.fold(atUrl)(_ + atUrl)
  }

}

