package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.connection.types.QueryNotificationConfirmedType
import org.ogf.schemas.nsi._2013._07.connection.types.NotificationBaseType
import org.ogf.schemas.nsi._2013._07.connection.types.QueryFailedType
import play.api._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Failure, Success }
import support.ExtraBodyParsers._

object ConnectionProvider extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)
  implicit def actorSystem = Akka.system

  private val requesterContinuations = new Continuations[NsiRequesterMessage[NsiRequesterOperation]]()
  private val pceContinuations = new Continuations[PceResponse]()

  def connectionFactory(connectionId: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve]): (ActorRef, ConnectionEntity) = {
    val outbound = outboundActor(initialReserve)
    val correlationIdGenerator = Uuid.deterministicUuidGenerator(connectionId.##)

    (outbound, new ConnectionEntity(connectionId, initialReserve, () => CorrelationId.fromUuid(correlationIdGenerator()), Configuration.Nsa, URI.create(ConnectionRequester.serviceUrl), URI.create(PathComputationEngine.pceReplyUrl)))
  }

  val connectionManager = new ConnectionManager(connectionFactory)

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionProvider.request().url}"

  def request = NsiProviderEndPoint {
    case message @ NsiProviderMessage(headers, query: NsiProviderQuery)     => handleQuery(query)(replyToClient(headers)).map(message.ack)
    case message @ NsiProviderMessage(headers, command: NsiProviderCommand) => handleCommand(message)(replyToClient(headers)).map(message.ack)
  }

  private def replyToClient(requestHeaders: NsiHeaders)(response: NsiRequesterOperation): Unit =
    requestHeaders.replyTo.foreach { replyTo =>
      val ack = NsiWebService.callRequester(ProviderEndPoint(requestHeaders.requesterNSA, replyTo, NoAuthentication), NsiRequesterMessage(requestHeaders.forSyncAck, response))

      ack.onComplete {
        case Failure(error)                                                 => Logger.info(s"Replying $response to $replyTo: $error", error)
        case Success(NsiRequesterMessage(headers, ServiceException(error))) => Logger.info(s"Replying $response to $replyTo: $error")
        case Success(acknowledgement)                                       => Logger.debug(s"Replying $response to $replyTo succeeded with $acknowledgement")
      }
    }

  private[controllers] def handleQuery(message: NsiProviderQuery)(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = message match {
    case q: QuerySummary =>
      val connectionStates = queryConnections(q.ids)
      connectionStates.onSuccess {
        case reservations => replyTo(QuerySummaryConfirmed(reservations))
      }
      Future.successful(GenericAck())
    case q: QuerySummarySync =>
      val connectionStates = queryConnections(q.ids)
      connectionStates map { states =>
        QuerySummarySyncConfirmed(states)
      }
    case q: QueryRecursive =>
      Future.successful(ServiceException(NsiError.NotImplemented.toServiceException(Configuration.Nsa)))
    case q: QueryNotification =>
      val connection = connectionManager.get(q.connectionId)
      connection.map { con =>
        queryNotifications(con, q.start, q.end) onSuccess {
          case n => replyTo(QueryNotificationConfirmed(n))
        }
      }
      Future.successful(connection.fold[NsiAcknowledgement](ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.Nsa)))(_ => GenericAck()))
    case q: QueryNotificationSync =>
      val connection = connectionManager.get(q.connectionId)
      val ack = connection.map(queryNotifications(_, q.start, q.end).map(QueryNotificationSyncConfirmed))

      ack.getOrElse(Future.successful(QueryNotificationSyncFailed(new QueryFailedType().withServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.Nsa)))))
  }

  private def queryNotifications(connection: ActorRef, start: Option[Int], end: Option[Int]): Future[Seq[NotificationBaseType]] = {
    val range = start.getOrElse(1) to end.getOrElse(Int.MaxValue)
    val notifications = (connection ? 'queryNotifications).mapTo[Seq[NotificationBaseType]]
    notifications.map(ns => ns.filter(n => range.contains(n.getNotificationId())))
  }

  private def queryConnections(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]): Future[Seq[QuerySummaryResultType]] = {
    val cs = ids match {
      case Some(Left(connectionIds))         => connectionManager.find(connectionIds)
      case Some(Right(globalReservationIds)) => connectionManager.findByGlobalReservationIds(globalReservationIds)
      case None                              => connectionManager.all
    }

    Future.traverse(cs)(c => (c ? 'query).mapTo[QuerySummaryResultType])
  }

  private[controllers] def handleCommand(request: NsiProviderMessage[NsiProviderOperation])(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] =
    connectionManager.findOrCreateConnection(request) match {
      case None =>
        Future.successful(ServiceException(NsiError.ConnectionNonExistent.toServiceException(Configuration.Nsa)))
      case Some(connectionActor) =>
        requesterContinuations.register(request.headers.correlationId).onSuccess {
          case reply => replyTo(reply.body)
        }
        (connectionActor ? FromRequester(request)).mapTo[NsiAcknowledgement]
    }

  def outboundActor(initialReserve: NsiProviderMessage[InitialReserve]) =
    Akka.system.actorOf(Props(new OutboundRoutingActor(ConnectionRequester.nsiRequester, PathComputationEngine.pceRequester, replyToClient(initialReserve.headers))))

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef, notify: NsiNotification => Unit) extends Actor {
    def receive = {
      case pceRequest: ToPce                     => pceRequester forward pceRequest
      case nsiRequest: ToProvider                => nsiRequester forward nsiRequest
      case ToRequester(NsiRequesterMessage(headers, message: NsiNotification)) => notify(message)
      case ToRequester(response)                 => handleResponse(response)
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterMessage[NsiRequesterOperation]): Unit =
    requesterContinuations.replyReceived(message.headers.correlationId, message)

}
