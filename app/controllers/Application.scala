package controllers

import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import akka.pattern.ask
import akka.util.Timeout
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.joda.time.DateTime
import nl.surfnet.safnari.ConnectionId
import nl.surfnet.safnari.ReservationState
import nl.surfnet.safnari.ConnectionData
import nl.surfnet.safnari._
import akka.actor.ActorRef

class Application(connectionManager: ConnectionManager) extends Controller {
  implicit val timeout = Timeout(2.seconds)

  private lazy val version: String = Seq("git", "log", "--format=%ad %h", "--date=short", "-1").!!

  def index = Action { implicit request =>
    val secure = request.headers.get("X-Forwarded-Proto") == Some("https")
    Ok(views.html.index(secure, Configuration.Nsa))
  }

  def healthcheck = Action.async {
    val pceHealth = (PathComputationEngine.pceRequester ? 'healthCheck).mapTo[Future[(String, Boolean)]].flatMap(identity)
    val nsiHealth = (ConnectionRequester.nsiRequester ? 'healthCheck).mapTo[Future[(String, Boolean)]].flatMap(identity)

    Future.sequence(List(nsiHealth, pceHealth)) map { healthStates =>
      val view = views.html.healthcheck(healthStates.toMap, version)

      if (healthStates forall { case (_, healthy) => healthy })
        Ok(view)
      else
        InternalServerError(view)
    }
  }

  def connections(page: Int) = Action.async {
    val timeBound = DateTime.now().minusWeeks(1).toXmlGregorianCalendar

    // FIXME data consistency (two messages may be interleaved with other messages)
    val queryResult = Future.traverse(connectionManager.all)(connectionDetails)

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

  def connection(id: ConnectionId) = Action.async {
    // FIXME data consistency (db query + two messages may be interleaved with other messages)
    connectionManager.get(id).map { c =>
      connectionDetails(c) map { case (connection, segments) =>
        val messages = connectionManager.messageStore.loadAll(id)
        Ok(views.html.connection(connection, segments, messages))
      }
    }.getOrElse {
      Future.successful(NotFound)
    }
  }

  private def connectionDetails(connection: ActorRef) = for {
    summary <- (connection ? 'query).mapTo[QuerySummaryResultType]
    segments <- (connection ? 'querySegments).mapTo[Seq[ConnectionData]]
  } yield {
    summary -> segments
  }

}
