package controllers

import play.api.mvc.Action
import play.api.mvc.AnyContent

object WebJarAssets {
  
  def at(file: String): Action[AnyContent] = {
    Assets.at("/META-INF/resources/webjars", file)
  }

}