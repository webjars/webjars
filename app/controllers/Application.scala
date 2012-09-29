package controllers

import play.api.mvc.{Action, Controller}

object Application extends Controller {
    
  def index = Action {
    Ok(views.html.index())
  }

  def documentation = Action {
    Ok(views.html.documentation())
  }

  def examples = Action {
    Ok(views.html.examples())
  }
  
}