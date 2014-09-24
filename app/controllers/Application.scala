package controllers

import akka.actor.ActorRef
import akka.pattern.ask
import controllers.ActorSupport._
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import presenters.{ConnectionPathSegmentPresenter, ConnectionPresenter}

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
    val now = DateTime.now
    val timeBound = now.minusWeeks(1).toXmlGregorianCalendar

    def inFuture(schedule: ScheduleType) = Option(schedule.getStartTime).exists(_.toDateTime.compareTo(now) > 0)
    def isActive(schedule: ScheduleType) = ! inFuture(schedule) && Option(schedule.getEndTime).map(_.toDateTime.compareTo(now) > 0).getOrElse(true)

    // FIXME data consistency (two messages may be interleaved with other messages)
    val queryResult = Future.traverse(connectionManager.all)(connectionDetails)

    queryResult map { cs =>
      val connections =
        cs.filter {
          case (criteria, _, _) =>
            val endTime = Option(criteria.getSchedule().getEndTime())
            endTime.forall(_.compare(timeBound) > 0)
        }.map {
          case (criteria, summary, segments) =>
            ( criteria, ConnectionPresenter(summary), segments.map{ ConnectionPathSegmentPresenter(_) } )
        }.sortBy {
          case (criteria, _, _) => Option(criteria.getSchedule().getStartTime())
        }.reverse.groupBy {
          case (criteria, _, _) if inFuture(criteria.getSchedule) => 'future
          case (criteria, _, _) if isActive(criteria.getSchedule) => 'active
          case _ => 'past
        }

      Ok(views.html.connections(connections.withDefaultValue(Nil), Configuration.WebParams))
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