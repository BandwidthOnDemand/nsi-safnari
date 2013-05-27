package controllers

import java.util.UUID
import java.net.URI
import scala.concurrent.stm._
import scala.concurrent.duration._
import scala.concurrent._
import play.api._
import play.api.mvc.{ Request => _, Response => _, _ }
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.current
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
import support.ExtraBodyParsers._
import models._
import nl.surfnet.safnari._
import scala.util.{ Failure, Success }
import com.twitter.bijection.Injection
import com.ning.http.client.Realm.AuthScheme

object ConnectionProvider extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)

  private val continuations = new Continuations[NsiRequesterOperation]()
  private val pceContinuations = new Continuations[PceResponse]()

  def connectionFactory(connectionId: ConnectionId, initialReserve: Reserve)(messageCallback: (ActorRef, Message, ActorRef) => Unit): ActorRef = {
    val outbound = outboundActor(initialReserve)
    val correlationIdGenerator = Uuid.deterministicUuidGenerator(connectionId.##)

    Akka.system.actorOf(Props(new ConnectionActor(connectionId, initialReserve, () => CorrelationId.fromUuid(correlationIdGenerator()), (message, sender) => {
      messageCallback(sender, message, outbound)
    }, URI.create(ConnectionRequester.serviceUrl), URI.create(ConnectionProvider.pceReplyUrl))))
  }

  val connectionManager = new ConnectionManager(connectionFactory)
  connectionManager.restore()

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionProvider.request().url}"

  private def pceReplyUrl: String = s"${Application.baseUrl}${routes.ConnectionProvider.pceReply().url}"

  def request = NsiProviderEndPoint {
    case query: NsiQuery     => handleQuery(query)(replyToClient(query.headers))
    case command: NsiCommand => handleRequest(command)(replyToClient(command.headers))
  }

  def pceReply = Action(parse.json) { implicit request =>
    Json.fromJson[PceResponse](request.body) match {
      case JsSuccess(response, _) =>
        pceContinuations.replyReceived(response.correlationId, response)
        Ok
      case JsError(error) =>
        BadRequest
    }
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
        val exception = new ServiceExceptionType()
          .withNsaId("MYNSAID") // TODO
          .withErrorId("UNKNOWN") // TODO
          .withText(f"Unknown connection ${connectionId}")
          .withVariables(null) // TODO
        Future.successful(ServiceException(request.headers.asReply, exception))
      case (connectionId, Some(connectionActor)) =>
        handleProviderOperation(request)(replyTo)(connectionActor)
    }
  }

  private def handleProviderOperation(message: NsiProviderOperation)(replyTo: NsiRequesterOperation => Unit)(connection: ActorRef): Future[NsiAcknowledgement] = {
    continuations.register(message.correlationId).onSuccess {
      case reply => replyTo(reply)
    }
    connection ? FromRequester(message) map (_.asInstanceOf[NsiAcknowledgement])
  }

  def outboundActor(initialReserve: Reserve) = {
    val pceRequester = {
      val pceEndpoint = current.configuration.getString("pce.endpoint").getOrElse(sys.error("pce.endpoint configuration property is not set"))
      current.configuration.getString("pce.actor") match {
        case None | Some("dummy") => Akka.system.actorOf(Props[DummyPceRequesterActor])
        case _                    => Akka.system.actorOf(Props(new PceRequesterActor(pceEndpoint)))
      }
    }
    val nsiRequester = {
      val requesterNsa = current.configuration.getString("safnari.requester.nsa").getOrElse(sys.error("safnari.requester.nsa configuration property is not set"))
      current.configuration.getString("nsi.actor") match {
        case None | Some("dummy") => Akka.system.actorOf(Props[DummyNsiRequesterActor])
        case _                    => Akka.system.actorOf(Props(new NsiRequesterActor(requesterNsa, URI.create(ConnectionRequester.serviceUrl))))
      }
    }

    Akka.system.actorOf(Props(new OutboundRoutingActor(nsiRequester, pceRequester, replyToClient(initialReserve.headers))))
  }

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef, notify: NsiNotification => Unit) extends Actor {
    def receive = {
      case pceRequest: ToPce                     => pceRequester forward pceRequest
      case nsiRequest: ToProvider                => nsiRequester forward nsiRequest
      case ToRequester(message: NsiNotification) => notify(message)
      case ToRequester(response)                 => handleResponse(response.pp("handle response"))
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterOperation): Unit =
    continuations.replyReceived(message.correlationId, message)

  class DummyNsiRequesterActor extends Actor {
    def receive = {
      case ToProvider(reserve: Reserve, _) =>
        sender ! FromProvider(ReserveConfirmed(reserve.headers.asReply, newConnectionId, Injection.invert(reserve.body.getCriteria()).get))
      case ToProvider(commit: ReserveCommit, _) =>
        sender ! FromProvider(ReserveCommitConfirmed(commit.headers.asReply, commit.connectionId))
    }
  }

  class NsiRequesterActor(requesterNsa: String, requesterUrl: URI) extends Actor {
    def receive = {
      case ToProvider(message: NsiProviderOperation, provider) =>
        val connection = sender
        ConnectionRequester.expectReplyFor(message.correlationId).onSuccess {
          case reply => connection ! FromProvider(reply)
        }

        var request = WS.url(provider.url.toASCIIString())

        request = provider.authentication match {
          case OAuthAuthentication(token)              => request.withHeaders("Authorization" -> s"bearer $token")
          case BasicAuthentication(username, password) => request.withAuth(username, password, AuthScheme.BASIC)
          case _                                       => request
        }

        request.post(message)
    }
  }

  class PceRequesterActor(endPoint: String) extends Actor {
    def receive = {
      case ToPce(request) =>
        val connection = sender
        pceContinuations.register(request.correlationId).onSuccess {
          case reply => connection ! FromPce(reply)
        }
        WS.url(endPoint).post(Json.toJson(request))
    }

    private def stpToJson(stp: StpType): JsValue =
      Json.obj("network-id" -> stp.getNetworkId(), "local-id" -> stp.getLocalId())
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case ToPce(pce: PathComputationRequest) =>
        sender !
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              pce.criteria.getPath().getSourceSTP,
              pce.criteria.getPath().getDestSTP(),
              ProviderEndPoint(
                "urn:ogf:network:nsa:surfnet.nl",
                URI.create("http://localhost:8082/bod/nsi/v2/provider"),
                OAuthAuthentication("4e54d76e-8767-4e6d-95a8-e2b7387e53bb"))))))
    }
  }

}
