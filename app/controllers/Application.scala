package controllers

import akka.actor.ActorRef
import akka.pattern.ask
import controllers.ActorSupport._
import nl.surfnet.safnari._
import org.joda.time.DateTime
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import presenters.{ConnectionPresenter, ConnectionPathSegmentPresenter}

import scala.concurrent.Future

class Application(connectionManager: ConnectionManager, pceRequester: ActorRef) extends Controller {
  def index = Action { implicit request =>
    val secure = request.headers.get(X_FORWARDED_PROTO) == Some("https")
    Ok(views.html.index(secure, Configuration.NsaId, Configuration.WebParams))
  }

  def healthcheck = Action.async {
    val pceHealth = (pceRequester ? 'healthCheck).mapTo[Future[(String, Boolean)]].flatMap(identity)
    val nsiHealth = (ConnectionRequester.nsiRequester ? 'healthCheck).mapTo[Future[(String, Boolean)]].flatMap(identity)

    Future.sequence(List(nsiHealth, pceHealth)) map { healthStates =>
      val view = views.html.healthcheck(healthStates.toMap, Configuration.VersionString, Configuration.WebParams)

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
        } map {
          case (criteria, summary, segments) =>
            ( criteria, ConnectionPresenter(summary), segments.map{ ConnectionPathSegmentPresenter(_) } )
        } sortBy {
          case (criteria, _, _) => Option(criteria.getSchedule().getStartTime())
        }

      Ok(views.html.connections(connections.reverse, Configuration.WebParams))
    }
  }

  def connection(id: ConnectionId) = Action.async {
    // FIXME data consistency (db query + two messages may be interleaved with other messages)
    connectionManager.get(id).map { c =>
      connectionDetails(c) map { case (criteria, summary, segments) =>
        val messages = connectionManager.messageStore.loadAll(id)
        Ok(views.html.connection(criteria, ConnectionPresenter(summary), segments.map{ ConnectionPathSegmentPresenter(_) }, messages, Configuration.WebParams))
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