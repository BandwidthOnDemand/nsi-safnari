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
import support.ExtraBodyParsers._
import models._
import nl.surfnet.safnari._
import scala.util.Failure
import scala.util.Success
import java.net.URL
import com.twitter.bijection.Injection
import com.ning.http.client.Realm.AuthScheme

object ConnectionProvider extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]
  private val continuations = new Continuations[NsiRequesterOperation]()
  private val pceContinuations = new Continuations[PceMessage]()

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionProvider.request().url}"

  def request = NsiProviderEndPoint {
    case NsiEnvelope(headers, query: NsiQuery) =>
      Future.successful(handleQuery(query)(replyToClient(headers)))
    case request @ NsiEnvelope(headers, _: NsiCommand) =>
      handleRequest(request)(replyToClient(headers))
  }

  def pceReply = Action(parse.json) { implicit request =>
    request.body.pp("PCE reply")
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
        post(NsiEnvelope(requestHeaders.asReply, response)).
        onComplete {
          case Failure(error) =>
            Logger.info(f"replying to $replyTo: $error", error)
          case Success(acknowledgement) =>
            Logger.debug(f"replying to $replyTo: ${acknowledgement.status} ${acknowledgement.statusText}")
        }
    }
  }

  private[controllers] def handleQuery(message: NsiQuery)(replyTo: NsiRequesterOperation => Unit): NsiAcknowledgement = message match {
    case q: QuerySummary =>
      val cs = connections.single.snapshot
      val connectionIds = if (q.connectionIds.isEmpty) cs.keys.toSeq else q.connectionIds
      val connectionStates = Future.sequence(connectionIds.flatMap { id =>
        cs.get(id).map(_ ? 'query map (_.asInstanceOf[QuerySummaryResultType]))
      })
      connectionStates.onSuccess {
        case reservations =>
          replyTo(QuerySummaryConfirmed(q.correlationId, reservations))
      }
      GenericAck(q.correlationId)
    case q => ???
  }

  private[controllers] def handleRequest(request: NsiEnvelope[NsiProviderOperation])(replyTo: NsiRequesterOperation => Unit): Future[NsiAcknowledgement] = {
    findOrCreateConnection(request) match {
      case Left(connectionId) =>
        Future.successful(ServiceException(request.body.correlationId, f"Unknown connection ${connectionId}"))
      case Right(connectionActor) =>
        handleProviderOperation(request.body)(replyTo)(connectionActor)
    }
  }

  private val uuidGenerator = Uuid.randomUuidGenerator()

  private def findOrCreateConnection(request: NsiEnvelope[NsiProviderOperation]): Either[ConnectionId, ActorRef] = request.body.optionalConnectionId match {
    case Some(connectionId) =>
      connections.single.get(connectionId).toRight(connectionId)
    case None =>
      // Initial reserve request.
      val connectionId = newConnectionId
      val connectionActor = Akka.system.actorOf(Props(new ConnectionActor(connectionId, request.headers.requesterNSA, () => CorrelationId.fromUuid(uuidGenerator()), outboundActor)))
      connections.single(connectionId) = connectionActor
      Right(connectionActor)
  }

  private def handleProviderOperation(message: NsiProviderOperation)(replyTo: NsiRequesterOperation => Unit)(connection: ActorRef): Future[NsiAcknowledgement] = {
    continuations.register(message.correlationId).onSuccess {
      case reply => replyTo(reply)
    }
    connection ? FromRequester(message) map (_.asInstanceOf[NsiAcknowledgement])
  }

  private def outboundActor = {
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

    Akka.system.actorOf(Props(new OutboundRoutingActor(nsiRequester, pceRequester)))
  }

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef) extends Actor {
    def receive = {
      case pceRequest: ToPce      => pceRequester forward pceRequest
      case nsiRequest: ToProvider => nsiRequester forward nsiRequest
      case ToRequester(response)  => handleResponse(response)
    }
  }

  class DummyNsiRequesterActor extends Actor {
    def receive = {
      case ToProvider(reserve: Reserve, _, _, _) =>
        sender ! FromProvider(ReserveConfirmed(reserve.correlationId, newConnectionId, Injection.invert(reserve.body.getCriteria()).get))
      case ToProvider(commit: ReserveCommit, _, _, _) =>
        sender ! FromProvider(ReserveCommitConfirmed(commit.correlationId, commit.connectionId))
    }
  }

  class NsiRequesterActor(requesterNsa: String, requesterUrl: URI) extends Actor {
    def receive = {
      case ToProvider(message: NsiProviderOperation, providerNsa, providerUrl, authentication) =>
        val connection = sender
        ConnectionRequester.expectReplyFor(message.correlationId).onSuccess {
          case reply => connection ! FromProvider(reply)
        }

        val headers = NsiHeaders(
          message.correlationId,
          requesterNsa,
          providerNsa,
          Some(requesterUrl))

        var request = WS.url(providerUrl.toASCIIString())

        request = authentication match {
          case OAuthAuthentication(token)              => request.withHeaders("Authorization" -> s"bearer $token")
          case BasicAuthentication(username, password) => request.withAuth(username, password, AuthScheme.BASIC)
          case _                                       => request
        }

        request.post(NsiEnvelope(headers, message))
    }
  }

  class PceRequesterActor(endPoint: String) extends Actor {
    def receive = {
      case ToPce(request) =>
        WS.url(endPoint).post(Json.toJson(request))
    }

    private def stpToJson(stp: StpType): JsValue =
      Json.obj("network-id" -> stp.getNetworkId(), "local-id" -> stp.getLocalId())
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case ToPce(pce) =>
        sender !
          FromPce(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              pce.criteria.getPath().getSourceSTP,
              pce.criteria.getPath().getDestSTP(),
              "urn:ogf:network:nsa:surfnet.nl",
              URI.create("http://localhost:8082/bod/nsi/v2/provider"),
              OAuthAuthentication("f44b1e47-0a19-4c11-861b-c9abf82d4cbf")))))
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterOperation): Unit =
    continuations.replyReceived(message.correlationId, message)
}
