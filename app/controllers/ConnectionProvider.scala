package controllers

import java.net.URI

import akka.actor._
import controllers.ActorSupport._
import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._
import soap.SoapWebService
import soap.ExtraBodyParsers._
import nl.surfnet.safnari._
import org.joda.time.Instant
import org.ogf.schemas.nsi._2013._12.connection.types._
import play.api.Play._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._

import scala.concurrent.Future
import scala.util.{Failure, Success}

class ConnectionProvider(connectionManager: ConnectionManager) extends Controller with SoapWebService {

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = ConnectionProvider.serviceUrl

  def request = NsiProviderEndPoint(Configuration.NsaId) {
    case message @ NsiProviderMessage(headers, query: QueryRecursive)       => handleQueryRecursive(NsiProviderMessage(headers, query))(ConnectionProvider.replyToClient(headers)).map(message.ack)
    case message @ NsiProviderMessage(headers, query: NsiProviderQuery)     => handleQuery(query, headers.requesterNSA)(ConnectionProvider.replyToClient(headers)).map(message.ack)
    case message @ NsiProviderMessage(headers, command: NsiProviderCommand) => handleCommand(NsiProviderMessage(headers, command))(ConnectionProvider.replyToClient(headers)).map(message.ack)
  }

  private[controllers] def handleCommand(request: NsiProviderMessage[NsiProviderCommand])(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] =
    connectionManager.findOrCreateConnection(request) match {
      case None =>
        Future.successful(ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.NsaId)))
      case Some(connection) =>
        ConnectionProvider.requesterContinuations.register(request.headers.correlationId, Configuration.AsyncReplyTimeout).onSuccess {
          case reply => replyTo(reply.body)
        }

        connection ? Connection.Command(new Instant(), FromRequester(request))
    }

  private[controllers] def handleQueryRecursive(message: NsiProviderMessage[QueryRecursive])(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = {
    val connections = connectionIdsToConnections(message.body.ids, message.headers.requesterNSA)

    val answers = connections.flatMap { cs =>
      Future.traverse(cs)(c => (c ? Connection.QueryRecursive(FromRequester(message))))
    }

    answers onComplete {
      case Failure(e) => println(s"Answers Future failed: $e")
      case Success(list) =>
        val resultTypes = list.flatMap {
          case ToRequester(NsiRequesterMessage(_, QueryRecursiveConfirmed(resultType))) => resultType
          case ToRequester(NsiRequesterMessage(_, Error(e)))                            => Seq.empty
        }

        replyTo(QueryRecursiveConfirmed(resultTypes))
    }

    Future.successful(GenericAck())
  }

  private[controllers] def handleQuery(query: NsiProviderQuery, nsaRequester: String)(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = query match {
    case q: QuerySummary =>
      queryConnections(q.ids, nsaRequester) onSuccess {
        case reservations => replyTo(QuerySummaryConfirmed(reservations))
      }
      Future.successful(GenericAck())
    case q: QuerySummarySync =>
      queryConnections(q.ids, nsaRequester) map { states =>
        QuerySummarySyncConfirmed(states)
      }
    case q: QueryNotification =>
      val connection = connectionManager.get(q.connectionId)
      connection.map { con =>
        queryNotifications(con, q.start, q.end) onSuccess {
          case n => replyTo(QueryNotificationConfirmed(n))
        }
      }
      Future.successful(connection.fold[NsiAcknowledgement](ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.NsaId)))(_ => GenericAck()))
    case q: QueryNotificationSync =>
      val ack = connectionManager.get(q.connectionId).map(queryNotifications(_, q.start, q.end).map(QueryNotificationSyncConfirmed))

      ack.getOrElse(Future.successful(ErrorAck(new GenericErrorType().withServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.NsaId)))))
    case q: QueryResult =>
      val connection = connectionManager.get(q.connectionId)
      connection.map { con =>
        queryResults(con, q.start, q.end) onSuccess {
          case n => replyTo(QueryResultConfirmed(n))
        }
      }

      Future.successful(connection.fold[NsiAcknowledgement](ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.NsaId)))(_ => GenericAck()))
    case q: QueryResultSync =>
      val ack = connectionManager.get(q.connectionId).map(queryResults(_, q.start, q.end).map(QueryResultSyncConfirmed))

      ack.getOrElse(Future.successful(ErrorAck(new GenericErrorType().withServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.NsaId)))))
    case q: QueryRecursive =>
      sys.error("Should be handled by its own handler")
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
      Future.traverse(cs)(c => (c ? Connection.Query).map(_._2))
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
    actorSystem.actorOf(Props(new OutboundRoutingActor(nsiRequester, pceRequester, replyToClient(initialReserve.headers))))

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef, notify: NsiNotification => Unit) extends Actor {
    def receive = {
      case pceRequest: ToPce                     => pceRequester forward pceRequest
      case nsiRequest: ToProvider                => nsiRequester forward nsiRequest
      case ToRequester(NsiRequesterMessage(headers, message: NsiNotification)) => notify(message)
      case ToRequester(response)                 => handleResponse(response)
    }
  }

  private def replyToClient(requestHeaders: NsiHeaders)(response: NsiRequesterOperation) = requestHeaders.replyTo.foreach { replyTo =>
    val ackFuture = NsiWebService.callRequester(ProviderEndPoint(requestHeaders.requesterNSA, replyTo), NsiRequesterMessage(requestHeaders.forSyncAck, response))

    ackFuture onComplete {
      case Failure(error)                                                 => Logger.info(s"Replying $response to $replyTo: $error", error)
      case Success(NsiRequesterMessage(headers, ServiceException(error))) => Logger.info(s"Replying $response to $replyTo: $error")
      case Success(acknowledgement)                                       => Logger.debug(s"Replying $response to $replyTo succeeded with $acknowledgement")
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterMessage[NsiRequesterOperation]): Unit =
    requesterContinuations.replyReceived(message.headers.correlationId, message)

}
