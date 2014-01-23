package controllers

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import play.api.Play.current
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.sys.process._

class Application(connectionManager: ConnectionManager) extends Controller {
  implicit val timeout = Timeout(2.seconds)

  def index = Action { implicit request =>
    val secure = request.headers.get("X-Forwarded-Proto") == Some("https")
    Ok(views.html.index(secure, Configuration.Nsa))
  }

  def healthcheck = Action.async {
    val pceHealth = (PathComputationEngine.pceRequester ? 'healthCheck).mapTo[Future[(String, Boolean)]].flatMap(identity)
    val nsiHealth = (ConnectionRequester.nsiRequester ? 'healthCheck).mapTo[Future[(String, Boolean)]].flatMap(identity)

    Future.sequence(List(nsiHealth, pceHealth)) map { healthStates =>
      val view = views.html.healthcheck(healthStates.toMap, s"${BuildInfo.version} (${BuildInfo.gitHeadCommitSha})")

      if (healthStates forall { case (_, healthy) => healthy })
        Ok(view)
      else
        InternalServerError(view)
    }
  }

  def connections = Action.async {
    val timeBound = DateTime.now().minusWeeks(1).toXmlGregorianCalendar

    // FIXME data consistency (two messages may be interleaved with other messages)
    val queryResult = Future.traverse(connectionManager.all)(connectionDetails)

    queryResult map { cs =>
      val connections =
        cs filter {
          case (criteria, _, _) =>
            val endTime = Option(criteria.getSchedule().getEndTime())
            endTime.map(_.compare(timeBound) > 0).getOrElse(true)
        } sortBy {
          case (criteria, _, _) => Option(criteria.getSchedule().getStartTime())
        }

      Ok(views.html.connections(connections.reverse))
    }
  }

  def connection(id: ConnectionId) = Action.async {
    // FIXME data consistency (db query + two messages may be interleaved with other messages)
    connectionManager.get(id).map { c =>
      connectionDetails(c) map { case (criteria, summary, segments) =>
        val messages = connectionManager.messageStore.loadAll(id)
        Ok(views.html.connection(criteria, summary, segments, messages))
      }
    }.getOrElse {
      Future.successful(NotFound(s"Connection ($id) was not found"))
    }
  }

  private def connectionDetails(connection: Connection) = for {
    (criteria, summary) <- (connection ? Connection.Query)
    segments <- (connection ? Connection.QuerySegments)
  } yield {
    (criteria, summary, segments)
  }

}