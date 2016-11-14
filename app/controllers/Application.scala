/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package controllers

import akka.actor.ActorRef
import akka.pattern.ask
import controllers.ActorSupport._
import java.time.ZonedDateTime
import java.time.temporal._
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._
import nl.surfnet.safnari._
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
    val now = ZonedDateTime.now
    val timeBound = now.minus(1, ChronoUnit.WEEKS)

    // FIXME data consistency (two messages may be interleaved with other messages)
    val queryResult = Future.traverse(connectionManager.all)(connectionDetails)

    queryResult map { cs =>
      val connections =
        cs.map {
          case (summary, pendingCriteria, segments) => (ConnectionPresenter(summary, pendingCriteria), segments.map{ ConnectionPathSegmentPresenter })
        }.filter {
          case (connection, _) => connection.endTime.fold2(_.compareTo(timeBound.toInstant) > 0, true, true)
        }.sortBy {
          case (connection, _) => connection.startTime.toOption(None)
        }.reverse.groupBy {
          case (connection, _) => connection.qualifier(now.toInstant)
        }

      Ok(views.html.connections(connections.withDefaultValue(Nil), Configuration.WebParams))
    }
  }

  def connection(id: ConnectionId) = Action.async {
    // FIXME data consistency (db query + two messages may be interleaved with other messages)
    connectionManager.get(id).map { c =>
      connectionDetails(c) map { case (summary, pendingCriteria, segments) =>
        val messages = connectionManager.messageStore.findByConnectionId(id)
        Ok(views.html.connection(ConnectionPresenter(summary, pendingCriteria), segments.map{ ConnectionPathSegmentPresenter }, messages, Configuration.WebParams))
      }
    }.getOrElse {
      Future.successful(NotFound(s"Connection ($id) was not found"))
    }
  }

  private def connectionDetails(connection: Connection) = for {
    queryResult <- (connection ? Connection.Query)
    segments <- (connection ? Connection.QuerySegments)
  } yield {
    (queryResult.summary, queryResult.pendingCriteria, segments)
  }

}
