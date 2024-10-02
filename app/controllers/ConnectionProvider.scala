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

import akka.actor._
import controllers.ActorSupport._
import java.net.URI
import java.time.Instant
import javax.inject._
import javax.xml.namespace.QName
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.soap._
import nl.surfnet.nsiv2.utils._
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._12.connection.types._
import play.api.Logger
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

@Singleton
class ConnectionProviderController @Inject() (
    connectionManager: ConnectionManager,
    connectionProvider: ConnectionProvider,
    configuration: Configuration,
    extraBodyParsers: ExtraBodyParsers,
    val actionBuilder: DefaultActionBuilder
)(implicit actorSystem: ActorSystem, ec: ExecutionContext)
    extends InjectedController
    with SoapWebService {
  private val logger = Logger(classOf[ConnectionProviderController])

  override val WsdlRoot = "wsdl/2.0"
  override val WsdlPath = ""
  override val WsdlBasename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = configuration.providerServiceUrl

  def request = extraBodyParsers.NsiProviderEndPoint(configuration.NsaId) { request =>
    validateRequest(request).map(Future.successful) getOrElse (request match {
      case message @ NsiProviderMessage(headers, query: NsiProviderQuery) =>
        handleQuery(NsiProviderMessage(headers, query))(
          connectionProvider.replyToClient(configuration, headers.replyTo)
        ).map(message.ack)
      case message @ NsiProviderMessage(headers, command: NsiProviderCommand) =>
        handleCommand(NsiProviderMessage(headers, command))(
          connectionProvider.replyToClient(configuration, headers.replyTo)
        ).map(message.ack)
    })
  }

  private def validateRequest(
      message: NsiProviderMessage[_]
  ): Option[NsiProviderMessage[ServiceException]] =
    validateRequesterNsa(message) orElse validateServiceType(message)

  private def validateRequesterNsa(
      message: NsiProviderMessage[_]
  ): Option[NsiProviderMessage[ServiceException]] = message.headers.replyTo.flatMap { replyTo =>
    val nsa = message.headers.requesterNSA
    if (configuration.Use2WayTLS) {
      configuration.translateToStunnelAddress(nsa, replyTo) match {
        case Success(_) =>
          None
        case Failure(_) =>
          logger.info(s"The requesterNSA '$nsa' does not match a known TLS NSA")
          val serviceException = ServiceException(
            NsiError.UnsupportedParameter
              .toServiceException(configuration.NsaId, NsiHeaders.REQUESTER_NSA -> nsa)
          )
          Some(message ack serviceException)
      }
    } else None
  }

  private def validateServiceType(
      message: NsiProviderMessage[_]
  ): Option[NsiProviderMessage[ServiceException]] = message match {
    case NsiProviderMessage(_, reserve: InitialReserve)
        if reserve.body.getCriteria.getServiceType eq null =>
      val x = message ack ServiceException(
        NsiError.MissingParameter
          .toServiceException(configuration.NsaId, new QName("serviceType") -> "is required")
      )
      Some(x)
    case _ =>
      None
  }

  private[controllers] def handleCommand(request: NsiProviderMessage[NsiProviderCommand])(
      sendAsyncReply: NsiRequesterMessage[NsiRequesterOperation] => Unit
  ): Future[NsiAcknowledgement] =
    connectionManager.findOrCreateConnection(request) match {
      case None =>
        Future.successful(
          ServiceException(NsiError.ReservationNonExistent.toServiceException(configuration.NsaId))
        )
      case Some(connection) =>
        connectionProvider.requesterContinuations
          .register(request.headers.correlationId, configuration.AsyncReplyTimeout)
          .foreach(sendAsyncReply)

        connection ? Connection.Command(Instant.now, FromRequester(request))
    }

  private[controllers] def handleQuery(message: NsiProviderMessage[NsiProviderQuery])(
      sendAsyncReply: NsiRequesterMessage[NsiRequesterOperation] => Unit
  ): Future[NsiAcknowledgement] = message.body match {
    case QuerySummary(ids, ifModifiedSince) =>
      queryConnections(ids, ifModifiedSince.map(_.toInstant)) onComplete {
        case Success((reservations, lastModifiedAt)) =>
          sendAsyncReply(
            message reply QuerySummaryConfirmed(
              reservations,
              lastModifiedAt.map(_.toXMLGregorianCalendar())
            )
          )
        case Failure(_) => // nothing
      }
      Future.successful(GenericAck())
    case QuerySummarySync(ids, ifModifiedSince) =>
      queryConnections(ids, ifModifiedSince.map(_.toInstant)) map { case (states, lastModifiedAt) =>
        QuerySummarySyncConfirmed(states, lastModifiedAt.map(_.toXMLGregorianCalendar()))
      }
    case QueryNotification(connectionId, start, end) =>
      val connection = connectionManager.get(connectionId)
      connection.map { con =>
        queryNotifications(con, start, end) onComplete {
          case Success(n) => sendAsyncReply(message reply QueryNotificationConfirmed(n))
          case Failure(_) => // nothing
        }
      }
      Future.successful(
        connection.fold[NsiAcknowledgement](
          ServiceException(NsiError.ReservationNonExistent.toServiceException(configuration.NsaId))
        )(_ => GenericAck())
      )
    case QueryNotificationSync(connectionId, start, end) =>
      val ack = connectionManager
        .get(connectionId)
        .map(queryNotifications(_, start, end).map(QueryNotificationSyncConfirmed))

      ack.getOrElse(
        Future.successful(
          ErrorAck(
            new GenericErrorType().withServiceException(
              NsiError.ReservationNonExistent.toServiceException(configuration.NsaId)
            )
          )
        )
      )
    case QueryResult(connectionId, start, end) =>
      val connection = connectionManager.get(connectionId)
      connection.map { con =>
        queryResults(con, start, end) onComplete {
          case Success(n) => sendAsyncReply(message reply QueryResultConfirmed(n))
          case Failure(_) => // nothing
        }
      }

      Future.successful(
        connection.fold[NsiAcknowledgement](
          ServiceException(NsiError.ReservationNonExistent.toServiceException(configuration.NsaId))
        )(_ => GenericAck())
      )
    case QueryResultSync(connectionId, start, end) =>
      val ack = connectionManager
        .get(connectionId)
        .map(queryResults(_, start, end).map(QueryResultSyncConfirmed))

      ack.getOrElse(
        Future.successful(
          ErrorAck(
            new GenericErrorType().withServiceException(
              NsiError.ReservationNonExistent.toServiceException(configuration.NsaId)
            )
          )
        )
      )
    case q @ QueryRecursive(_, _) =>
      handleQueryRecursive(message.copy(body = q))(sendAsyncReply)
  }

  private[controllers] def handleQueryRecursive(message: NsiProviderMessage[QueryRecursive])(
      sendAsyncReply: NsiRequesterMessage[NsiRequesterOperation] => Unit
  ): Future[NsiAcknowledgement] = {
    val connections = connectionIdsToConnections(message.body.ids)

    val answers =
      Future.traverse(connections)(c => (c ? Connection.QueryRecursive(FromRequester(message))))

    answers onComplete {
      case Failure(e) => println(s"Answers Future failed: $e")
      case Success(list) =>
        val resultTypes = list.flatMap {
          case ToRequester(NsiRequesterMessage(_, QueryRecursiveConfirmed(resultType))) =>
            resultType
          case ToRequester(NsiRequesterMessage(_, ErrorReply(_))) => Seq.empty
        }

        sendAsyncReply(message reply QueryRecursiveConfirmed(resultTypes))
    }

    Future.successful(GenericAck())
  }

  private def queryNotifications(
      connection: Connection,
      start: Option[Long],
      end: Option[Long]
  ): Future[Seq[NotificationBaseType]] = {
    val range = start.getOrElse(1L) to end.getOrElse(Long.MaxValue)
    val notifications = (connection ? Connection.QueryNotifications)
    notifications.map(ns => ns.filter(n => range.contains(n.getNotificationId())))
  }

  private def queryResults(
      connection: Connection,
      start: Option[Long],
      end: Option[Long]
  ): Future[Seq[QueryResultResponseType]] = {
    val range = start.getOrElse(1L) to end.getOrElse(Long.MaxValue)
    val results = (connection ? Connection.QueryResults)
    results.map(rs => rs.filter(r => range.contains(r.getResultId())))
  }

  private def queryConnections(
      ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]],
      ifModifiedSince: Option[Instant]
  ): Future[(Seq[QuerySummaryResultType], Option[Instant])] = {
    val maxInstant = (a: Instant, b: Instant) => if (a.isBefore(b)) b else a
    val connections = connectionIdsToConnections(ids)
    Future
      .traverse(connections)(c =>
        (c ? Connection.Query).filter(c => ifModifiedSince.fold(true)(_.isBefore(c.lastModifiedAt)))
      )
      .map(cs =>
        cs.map(_.summary) -> cs
          .map(_.lastModifiedAt)
          .reduceOption(maxInstant)
          .orElse(ifModifiedSince)
      )
  }

  private def connectionIdsToConnections(
      ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]
  ): Seq[Connection] = ids match {
    case Some(Left(connectionIds)) => connectionManager.find(connectionIds)
    case Some(Right(globalReservationIds)) =>
      connectionManager.findByGlobalReservationIds(globalReservationIds)
    case None => connectionManager.all
  }

}

@Singleton
class ConnectionProvider @Inject() (nsiWebService: NsiWebService)(implicit
    actorSystem: ActorSystem,
    ec: ExecutionContext
) {
  private val logger = Logger(classOf[ConnectionProvider])

  val requesterContinuations =
    new Continuations[NsiRequesterMessage[NsiRequesterOperation]](actorSystem.scheduler)

  def connectionFactory(
      createOutboundActor: NsiProviderMessage[InitialReserve] => ActorRef,
      configuration: Configuration
  )(
      connectionId: ConnectionId,
      initialReserve: NsiProviderMessage[InitialReserve]
  ): (ActorRef, ConnectionEntity) = {
    val outbound = createOutboundActor(initialReserve)
    val correlationIdGenerator = Uuid.deterministicUuidGenerator(connectionId.##)

    (
      outbound,
      new ConnectionEntity(
        configuration.NsaId,
        connectionId,
        initialReserve,
        () => CorrelationId.fromUuid(correlationIdGenerator()),
        configuration.PceAlgorithm,
        URI.create(configuration.requesterServiceUrl),
        URI.create(configuration.pceReplyUrl)
      )
    )
  }

  def outboundActor(
      configuration: Configuration,
      nsiRequester: => ActorRef,
      pceRequester: ActorRef
  )(initialReserve: NsiProviderMessage[InitialReserve]) =
    actorSystem.actorOf(
      Props(
        new OutboundRoutingActor(
          nsiRequester,
          pceRequester,
          replyToClient(configuration, initialReserve.headers.replyTo)
        )
      )
    )

  class OutboundRoutingActor(
      nsiRequester: ActorRef,
      pceRequester: ActorRef,
      notify: NsiRequesterMessage[NsiRequesterOperation] => Unit
  ) extends Actor {
    def receive = {
      case pceRequest: ToPce      => pceRequester forward pceRequest
      case nsiRequest: ToProvider => nsiRequester forward nsiRequest
      case ToRequester(notification @ NsiRequesterMessage(_, _: NsiNotification)) =>
        notify(notification)
      case ToRequester(response) => handleResponse(response)
    }
  }

  def replyToClient(configuration: Configuration, replyTo: Option[URI])(
      response: NsiRequesterMessage[NsiRequesterOperation]
  ): Unit = replyTo.foreach { replyTo =>
    val ackFuture =
      nsiWebService.callRequester(response.headers.requesterNSA, replyTo, response, configuration)

    ackFuture onComplete {
      case Failure(error) => logger.info(s"Replying $response to $replyTo: $error", error)
      case Success(NsiRequesterMessage(_, ServiceException(error))) =>
        logger.info(s"Replying $response to $replyTo: $error")
      case Success(acknowledgement) =>
        logger.debug(s"Replying $response to $replyTo succeeded with $acknowledgement")
    }
  }

  def handleResponse(message: NsiRequesterMessage[NsiRequesterOperation]): Unit =
    requesterContinuations.replyReceived(message.headers.correlationId, message)

}
