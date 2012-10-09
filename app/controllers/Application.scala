package controllers

import play.api.mvc.{Action, Controller}
import models.WebJars

object Application extends Controller {
    
  def index = Action {
    Ok(views.html.index(WebJars.all))
  }

  def documentation = Action {
    Ok(views.html.documentation())
  }
  
}