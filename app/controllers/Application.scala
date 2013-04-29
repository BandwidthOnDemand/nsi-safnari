package controllers

import play.api.Play.current
import play.api._
import play.api.mvc._

object Application extends Controller {

  def baseUrl = current.configuration.getString("nsi.base.url").getOrElse(sys.error("nsi.base.url option is not set"))

  def index = Action {
    Ok(views.html.index())
  }

}
