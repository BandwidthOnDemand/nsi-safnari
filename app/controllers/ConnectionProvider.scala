package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import play.api._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.WS
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{ Failure, Success }
import support.ExtraBodyParsers._
import SoapRequests._

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
      val request = WS.url(replyTo.toASCIIString()).withSoapActionHeader(response.soapActionUrl)

      request.post(NsiRequesterMessage(requestHeaders.forAsyncReply, response)).onComplete {
        case Failure(error)           => Logger.info(s"Replying to $replyTo: $error", error)
        case Success(acknowledgement) => Logger.debug(s"Replying $response to $replyTo")
      }
    }

  private[controllers] def handleQuery(message: NsiProviderQuery)(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = message match {
    case q: QuerySummary =>
      val connectionStates = queryConnections(q.connectionIds)
      connectionStates.onSuccess {
        case reservations =>
          replyTo(QuerySummaryConfirmed(reservations))
      }
      Future.successful(GenericAck())
    case q: QuerySummarySync =>
      val connectionStates = queryConnections(q.connectionIds)
      connectionStates map { states =>
        QuerySummarySyncConfirmed(states)
      }
    case q: QueryRecursive =>
      Future.successful(ServiceException(NsiError.NotImplemented.toServiceException(Configuration.Nsa)))
    case q: QueryNotification =>
      Future.successful(ServiceException(NsiError.NotImplemented.toServiceException(Configuration.Nsa)))
  }

  private def queryConnections(connectionIds: Seq[ConnectionId]) = {
    val cs = if (connectionIds.isEmpty) connectionManager.all else connectionManager.find(connectionIds)
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
