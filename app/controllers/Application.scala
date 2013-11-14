package controllers

import play.api.Play.current
import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.joda.time.DateTime
import nl.surfnet.safnari.ConnectionId
import nl.surfnet.safnari.ReservationState
import nl.surfnet.safnari.ConnectionData
import nl.surfnet.safnari._

object Application extends Controller {
  implicit val timeout = Timeout(2.seconds)

  def baseUrl = current.configuration.getString("nsi.base.url").getOrElse(sys.error("nsi.base.url option is not set"))

  def index = Action { implicit request =>
    val secure = request.headers.get("X-Forwarded-Proto") == Some("https")
    Ok(views.html.index(secure))
  }

  def connections(page: Int) = Action.async {
    val timeBound = DateTime.now().minusWeeks(1).toXmlGregorianCalendar

    val queryResult = Future.traverse(ConnectionProvider.connectionManager.all) { c =>
      (c ? 'query).mapTo[QuerySummaryResultType] flatMap { summary =>
        (c ? 'querySegments).mapTo[Seq[ConnectionData]] map (summary -> _)
      }
    }

    queryResult map { cs =>
      val connections =
        cs filter {
          case (con, _) =>
            val endTime = Option(con.getCriteria().get(0).getSchedule().getEndTime())
            endTime.map(_.compare(timeBound) > 0).getOrElse(true)
        } sortBy {
          case (con, _) => Option(con.getCriteria().get(0).getSchedule().getStartTime())
        }

      Ok(views.html.connections(connections.reverse))
    }
  }

}
