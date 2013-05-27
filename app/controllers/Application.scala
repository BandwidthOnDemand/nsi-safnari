package controllers

import play.api.Play.current
import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import nl.surfnet.safnari.ConnectionId
import nl.surfnet.safnari.ReservationState
import nl.surfnet.safnari.ConnectionData

object Application extends Controller {
  implicit val timeout = Timeout(2.seconds)

  def baseUrl = current.configuration.getString("nsi.base.url").getOrElse(sys.error("nsi.base.url option is not set"))

  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def connections = Action {
    Async {
      Future.traverse(ConnectionProvider.connectionManager.all) { c =>
        (c ? 'query).map (_.asInstanceOf[QuerySummaryResultType]).flatMap(summary => c ? 'querySegments map (segs => (summary -> segs.asInstanceOf[Seq[ConnectionData]])))
      }.map { cs =>
        Ok(views.html.connections(cs))
      }
    }

  }

}

