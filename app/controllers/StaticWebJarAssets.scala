package controllers

import javax.inject.Inject

import controllers.routes.{StaticWebJarAssets => AssetRoutes}
import play.api.Configuration
import play.api.mvc.Controller

class StaticWebJarAssets @Inject() (configuration: Configuration, webJarAssets: WebJarAssets) extends Controller {

  def at(file: String) = Assets.at("/META-INF/resources/webjars", file)
  def atBower(file: String) = Assets.at("/META-INF/resources/webjars", file)

  lazy val maybeContentUrl = configuration.getString("contentUrl")

  def getUrl(file: String) = {
    val atUrl = AssetRoutes.at(webJarAssets.locate(file)).url
    maybeContentUrl.fold(atUrl)(_ + atUrl)
  }

  def getUrlBower(file: String) = {
    val atUrl = AssetRoutes.atBower(webJarAssets.locate(file)).url
    maybeContentUrl.fold(atUrl)(_ + atUrl)
  }

}

