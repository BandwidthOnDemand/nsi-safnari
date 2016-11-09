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

import java.net.URI

import akka.actor._
import controllers.ActorSupport._
import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._
import soap.SoapWebService
import soap.ExtraBodyParsers._
import nl.surfnet.safnari._
import java.time.Instant
import org.ogf.schemas.nsi._2013._12.connection.types._
import play.api.Play._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._

import scala.concurrent.Future
import scala.util.{Failure, Success}

class ConnectionProvider(connectionManager: ConnectionManager) extends Controller with SoapWebService {

  override val WsdlRoot = "wsdl/2.0"
  override val WsdlPath = ""
  override val WsdlBasename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = ConnectionProvider.serviceUrl

  def request = NsiProviderEndPoint(Configuration.NsaId) { request =>
    validateRequesterNsa(request).map(Future.successful) getOrElse (request match {
      case message @ NsiProviderMessage(headers, query: NsiProviderQuery) => handleQuery(NsiProviderMessage(headers, query))(ConnectionProvider.replyToClient(headers.replyTo)).map(message.ack)
      case message @ NsiProviderMessage(headers, command: NsiProviderCommand) => handleCommand(NsiProviderMessage(headers, command))(ConnectionProvider.replyToClient(headers.replyTo)).map(message.ack)
    })
  }

  private def validateRequesterNsa(message: NsiProviderMessage[_]): Option[NsiProviderMessage[NsiAcknowledgement]] = message.headers.replyTo.flatMap { replyTo =>
    val nsa = message.headers.requesterNSA
    if (Configuration.Use2WayTLS) {
      Configuration.translateToStunnelAddress(nsa, replyTo) match {
        case Success(_) =>
          None
        case Failure(e) =>
          Logger.info(s"The requesterNSA '$nsa' does not match a known TLS NSA")
          val serviceException = ServiceException(NsiError.UnsupportedParameter.toServiceException(nsa, NsiHeaders.REQUESTER_NSA -> nsa))
          Some(message ack serviceException)
      }
    } else None
  }

  private[controllers] def handleCommand(request: NsiProviderMessage[NsiProviderCommand])(sendAsyncReply: NsiRequesterMessage[NsiRequesterOperation] => Unit): Future[NsiAcknowledgement] =
    connectionManager.findOrCreateConnection(request) match {
      case None =>
        Future.successful(ServiceException(NsiError.ReservationNonExistent.toServiceException(Configuration.NsaId)))
      case Some(connection) =>
        ConnectionProvider.requesterContinuations.register(request.headers.correlationId, Configuration.AsyncReplyTimeout).foreach(sendAsyncReply)

        connection ? Connection.Command(Instant.now, FromRequester(request))
    }

  private[controllers] def handleQuery(message: NsiProviderMessage[NsiProviderQuery])(sendAsyncReply: NsiRequesterMessage[NsiRequesterOperation] => Unit): Future[NsiAcknowledgement] = message.body match {
    case QuerySummary(ids) =>
      queryConnections(ids, message.headers.requesterNSA) onSuccess {
        case reservations => sendAsyncReply(message reply QuerySummaryConfirmed(reservations))
      }
      Future.successful(GenericAck())
    case QuerySummarySync(ids) =>
      queryConnections(ids, message.headers.requesterNSA) map { states =>
        QuerySummarySyncConfirmed(states)
      }
    case QueryNotification(connectionId, start, end) =>
      val connection = connectionManager.get(connectionId)
      connection.map { con =>
        queryNotifications(con, start, end) onSuccess {
          case n => sendAsyncReply(message reply QueryNotificationConfirmed(n))
        }
      }
      Future.successful(connection.fold[NsiAcknowledgement](ServiceException(NsiError.ReservationNonExistent.toServiceException(Configuration.NsaId)))(_ => GenericAck()))
    case QueryNotificationSync(connectionId, start, end) =>
      val ack = connectionManager.get(connectionId).map(queryNotifications(_, start, end).map(QueryNotificationSyncConfirmed))

      ack.getOrElse(Future.successful(ErrorAck(new GenericErrorType().withServiceException(NsiError.ReservationNonExistent.toServiceException(Configuration.NsaId)))))
    case QueryResult(connectionId, start, end) =>
      val connection = connectionManager.get(connectionId)
      connection.map { con =>
        queryResults(con, start, end) onSuccess {
          case n => sendAsyncReply(message reply QueryResultConfirmed(n))
        }
      }

      Future.successful(connection.fold[NsiAcknowledgement](ServiceException(NsiError.ReservationNonExistent.toServiceException(Configuration.NsaId)))(_ => GenericAck()))
    case QueryResultSync(connectionId, start, end) =>
      val ack = connectionManager.get(connectionId).map(queryResults(_, start, end).map(QueryResultSyncConfirmed))

      ack.getOrElse(Future.successful(ErrorAck(new GenericErrorType().withServiceException(NsiError.ReservationNonExistent.toServiceException(Configuration.NsaId)))))
    case q @ QueryRecursive(_) =>
      handleQueryRecursive(message.copy(body = q))(sendAsyncReply)
  }

  private[controllers] def handleQueryRecursive(message: NsiProviderMessage[QueryRecursive])(sendAsyncReply: NsiRequesterMessage[NsiRequesterOperation] => Unit): Future[NsiAcknowledgement] = {
    val connections = connectionIdsToConnections(message.body.ids, message.headers.requesterNSA)

    val answers = connections.flatMap { cs =>
      Future.traverse(cs)(c => (c ? Connection.QueryRecursive(FromRequester(message))))
    }

    answers onComplete {
      case Failure(e) => println(s"Answers Future failed: $e")
      case Success(list) =>
        val resultTypes = list.flatMap {
          case ToRequester(NsiRequesterMessage(_, QueryRecursiveConfirmed(resultType))) => resultType
          case ToRequester(NsiRequesterMessage(_, ErrorReply(e)))                       => Seq.empty
        }

        sendAsyncReply(message reply QueryRecursiveConfirmed(resultTypes))
    }

    Future.successful(GenericAck())
  }

  private def queryNotifications(connection: Connection, start: Option[Int], end: Option[Int]): Future[Seq[NotificationBaseType]] = {
    val range = start.getOrElse(1) to end.getOrElse(Int.MaxValue)
    val notifications = (connection ? Connection.QueryNotifications)
    notifications.map(ns => ns.filter(n => range.contains(n.getNotificationId())))
  }

  private def queryResults(connection: Connection, start: Option[Int], end: Option[Int]): Future[Seq[QueryResultResponseType]] = {
    val range = start.getOrElse(1) to end.getOrElse(Int.MaxValue)
    val results = (connection ? Connection.QueryResults)
    results.map(rs => rs.filter(r => range.contains(r.getResultId())))
  }

  private def queryConnections(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]], requesterNsa: String): Future[Seq[QuerySummaryResultType]] = {
    val connections = connectionIdsToConnections(ids, requesterNsa)

    connections flatMap { cs =>
      Future.traverse(cs)(c => (c ? Connection.Query).map(_.summary))
    }
  }

  private def connectionIdsToConnections(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]], requesterNsa: String): Future[Seq[Connection]] = ids match {
    case Some(Left(connectionIds))         => Future.successful(connectionManager.find(connectionIds))
    case Some(Right(globalReservationIds)) => Future.successful(connectionManager.findByGlobalReservationIds(globalReservationIds))
    case None                              => connectionManager.findByRequesterNsa(requesterNsa)
  }

}

object ConnectionProvider {
  private val requesterContinuations = new Continuations[NsiRequesterMessage[NsiRequesterOperation]](actorSystem.scheduler)

  def serviceUrl: String = s"${Configuration.BaseUrl}${routes.ConnectionProvider.request().url}"

  def connectionFactory(createOutboundActor: NsiProviderMessage[InitialReserve] => ActorRef)(connectionId: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve]): (ActorRef, ConnectionEntity) = {
    val outbound = createOutboundActor(initialReserve)
    val correlationIdGenerator = Uuid.deterministicUuidGenerator(connectionId.##)

    (outbound, new ConnectionEntity(connectionId, initialReserve, () => CorrelationId.fromUuid(correlationIdGenerator()), Configuration.NsaId, Configuration.PceAlgorithm, URI.create(ConnectionRequester.serviceUrl), URI.create(PathComputationEngine.pceReplyUrl)))
  }

  def outboundActor(nsiRequester: => ActorRef, pceRequester: ActorRef)(initialReserve: NsiProviderMessage[InitialReserve]) =
    actorSystem.actorOf(Props(new OutboundRoutingActor(nsiRequester, pceRequester, replyToClient(initialReserve.headers.replyTo))))

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef, notify: NsiRequesterMessage[NsiRequesterOperation] => Unit) extends Actor {
    def receive = {
      case pceRequest: ToPce                     => pceRequester forward pceRequest
      case nsiRequest: ToProvider                => nsiRequester forward nsiRequest
      case ToRequester(notification @ NsiRequesterMessage(_, _: NsiNotification)) => notify(notification)
      case ToRequester(response)                 => handleResponse(response)
    }
  }

  private def replyToClient(replyTo: Option[URI])(response: NsiRequesterMessage[NsiRequesterOperation]): Unit = replyTo.foreach { replyTo =>
    val ackFuture = NsiWebService.callRequester(response.headers.requesterNSA, replyTo, response)

    ackFuture onComplete {
      case Failure(error)                                                 => Logger.info(s"Replying $response to $replyTo: $error", error)
      case Success(NsiRequesterMessage(headers, ServiceException(error))) => Logger.info(s"Replying $response to $replyTo: $error")
      case Success(acknowledgement)                                       => Logger.debug(s"Replying $response to $replyTo succeeded with $acknowledgement")
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterMessage[NsiRequesterOperation]): Unit =
    requesterContinuations.replyReceived(message.headers.correlationId, message)

}
