package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
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

object ConnectionProvider extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)
  implicit def actorSystem = Akka.system

  private val requesterContinuations = new Continuations[NsiRequesterOperation]()
  private val pceContinuations = new Continuations[PceResponse]()

  def connectionFactory(connectionId: ConnectionId, initialReserve: Reserve): (ActorRef, ConnectionEntity) = {
    val outbound = outboundActor(initialReserve)
    val correlationIdGenerator = Uuid.deterministicUuidGenerator(connectionId.##)

    (outbound, new ConnectionEntity(connectionId, initialReserve, () => CorrelationId.fromUuid(correlationIdGenerator()), URI.create(ConnectionRequester.serviceUrl), URI.create(PathComputationEngine.pceReplyUrl)))
  }

  val connectionManager = new ConnectionManager(connectionFactory)

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionProvider.request().url}"

  def request = NsiProviderEndPoint {
    case query: NsiQuery     => handleQuery(query)(replyToClient(query.headers))
    case command: NsiCommand => handleRequest(command)(replyToClient(command.headers))
  }

  private def replyToClient(requestHeaders: NsiHeaders)(response: NsiRequesterOperation): Unit = {
    requestHeaders.replyTo.foreach { replyTo =>
      WS.url(replyTo.toASCIIString()).
        post(response).
        onComplete {
          case Failure(error) =>
            Logger.info(s"Replying to $replyTo: $error", error)
          case Success(acknowledgement) =>
            Logger.debug(s"Replying $response to $replyTo")
        }
    }
  }

  private[controllers] def handleQuery(message: NsiQuery)(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = message match {
    case q: QuerySummary =>
      val connectionStates = queryConnections(q.connectionIds)
      connectionStates.onSuccess {
        case reservations =>
          replyTo(QuerySummaryConfirmed(q.headers.asReply, reservations))
      }
      Future.successful(GenericAck(q.headers.asReply))
    case q: QuerySummarySync =>
      val connectionStates = queryConnections(q.connectionIds)
      connectionStates map { states =>
        QuerySummarySyncConfirmed(q.headers.asReply, states)
      }
    case q => ???
  }

  private def queryConnections(connectionIds: Seq[ConnectionId]) = {
    val cs = if (connectionIds.isEmpty) connectionManager.all else connectionManager.find(connectionIds)
    Future.traverse(cs)(c => c ? 'query map (_.asInstanceOf[QuerySummaryResultType]))
  }

  private[controllers] def handleRequest(request: NsiProviderOperation)(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = {
    connectionManager.findOrCreateConnection(request) match {
      case (connectionId, None) =>
        Future.successful(ServiceException(request.headers.asReply, NsiError.DoesNotExist.toServiceException(Configuration.Nsa)))
      case (connectionId, Some(connectionActor)) =>
        requesterContinuations.register(request.correlationId).onSuccess {
          case reply => replyTo(reply)
        }
        (connectionActor ? FromRequester(request)).mapTo[NsiAcknowledgement]
    }
  }

  def outboundActor(initialReserve: Reserve) =
    Akka.system.actorOf(Props(new OutboundRoutingActor(ConnectionRequester.nsiRequester, PathComputationEngine.pceRequester, replyToClient(initialReserve.headers))))

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef, notify: NsiNotification => Unit) extends Actor {
    def receive = {
      case pceRequest: ToPce                     => pceRequester forward pceRequest
      case nsiRequest: ToProvider                => nsiRequester forward nsiRequest
      case ToRequester(message: NsiNotification) => notify(message)
      case ToRequester(response)                 => handleResponse(response)
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterOperation): Unit =
    requesterContinuations.replyReceived(message.correlationId, message)

}
