package controllers

import java.util.UUID
import java.net.URI
import scala.concurrent.stm._
import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api._
import play.api.mvc.{ Request => _, Response => _, _ }
import play.api.libs.concurrent.Akka
import play.api.libs.ws.WS
import play.api.libs.json.{ JsObject, Json }
import play.api.Play.current
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import support.ExtraBodyParsers._
import models._
import nl.surfnet.nsi._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import scala.util.Failure
import scala.util.Success
import java.net.URL

object ConnectionProvider extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]
  private val continuations = TMap.empty[CorrelationId, NsiRequesterOperation => Unit]

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionProvider.request().absoluteURL()

  private def replyToClient(requestHeaders: NsiHeaders)(response: NsiRequesterOperation): Unit = {
    requestHeaders.replyTo.foreach { replyTo =>
      WS.url(replyTo.toASCIIString()).
        post(NsiEnvelope(requestHeaders.asReply, response)).
        onComplete {
          case Failure(error) =>
            Logger.info(f"replying to $replyTo: $error", error)
          case Success(response) =>
            Logger.debug(f"replying to $replyTo: ${response.status} ${response.statusText}")
        }
    }
  }

  private def findOrCreateConnection(request: NsiEnvelope[NsiProviderOperation]): Either[ConnectionId, ActorRef] = request.body.optionalConnectionId match {
    case Some(connectionId) =>
      connections.single.get(connectionId).toRight(connectionId)
    case None =>
      // Initial reserve request.
      val connectionId = newConnectionId
      val connectionActor = Akka.system.actorOf(Props(new ConnectionActor(connectionId, request.headers.requesterNSA, Uuid.randomUuidGenerator(), outboundActor)))
      connections.single(connectionId) = connectionActor
      Right(connectionActor)
  }

  def request = NsiEndPoint({
    case NsiEnvelope(headers, query: NsiQuery) =>
      Future.successful(handleQuery(query)(replyToClient(headers)))
    case request @ NsiEnvelope(headers, _: NsiProviderOperation) =>
      handleRequest(request)(replyToClient(headers))
  })

  private[controllers] def handleQuery(message: NsiQuery)(replyTo: NsiRequesterOperation => Unit): NsiResponseMessage = message match {
    case q: NsiProviderOperation.QuerySummary =>
      val cs = connections.single.snapshot
      val connectionStates = Future.sequence(q.connectionIds.flatMap { id =>
        cs.get(id).map(_ ? 'query map (_.asInstanceOf[QuerySummaryResultType]))
      })
      connectionStates.onSuccess {
        case reservations =>
          replyTo(NsiRequesterOperation.QuerySummaryConfirmed(q.correlationId, reservations))
      }
      NsiResponseMessage.GenericAck(q.correlationId)
    case q => ???
  }

  private[controllers] def handleRequest(request: NsiEnvelope[NsiProviderOperation])(replyTo: NsiRequesterOperation => Unit): Future[NsiResponseMessage] = {
    findOrCreateConnection(request) match {
      case Left(connectionId) =>
        Future.successful(NsiResponseMessage.ServiceException(request.body.correlationId, f"Unknown connection ${connectionId}"))
      case Right(connectionActor) =>
        handleProviderOperation(request.body)(replyTo)(connectionActor)
    }
  }

  private def handleProviderOperation(message: NsiProviderOperation)(replyTo: NsiRequesterOperation => Unit)(connection: ActorRef): Future[NsiResponseMessage] = {
    continuations.single(message.correlationId) = replyTo
    connection ? Inbound(message) map (_.asInstanceOf[NsiResponseMessage])
  }

  private def outboundActor = {
    val (nsiRequester, pceRequester) = {
      if (current.mode == Mode.Prod) {
        val pceEndpoint = current.configuration.getString("pce.endpoint").getOrElse(sys.error("pce.endpoint configuration property is not set"))
        (Akka.system.actorOf(Props[NsiRequesterActor]), Akka.system.actorOf(Props(new PceRequesterActor(pceEndpoint))))
      } else
        (Akka.system.actorOf(Props[DummyNsiRequesterActor]), Akka.system.actorOf(Props[DummyPceRequesterActor]))
    }

    Akka.system.actorOf(Props(new OutboundRoutingActor(nsiRequester, pceRequester)))
  }

  class OutboundRoutingActor(nsiRequester: ActorRef, pceRequester: ActorRef) extends Actor {
    def receive = {
      case pceRequest: PathComputationRequest => pceRequester forward pceRequest
      case nsiRequest: NsiProviderOperation   => nsiRequester forward nsiRequest
      case response: NsiRequesterOperation    => handleResponse(response)
    }
  }

  class DummyNsiRequesterActor extends Actor {
    def receive = {
      case reserve: Reserve =>
        sender ! Inbound(ReserveConfirmed(reserve.correlationId, newConnectionId))
      case commit: ReserveCommit =>
        sender ! Inbound(ReserveCommitConfirmed(commit.correlationId, commit.connectionId))
    }
  }

  class NsiRequesterActor extends Actor {
    def receive = {
      case reserve: Reserve => ???
      case commit: ReserveCommit => ???
    }
  }

  class PceRequesterActor(endPoint: String) extends Actor {
    def receive = {
      case PathComputationRequest(correlationId, criteria) =>
        WS.url(endPoint).post(Json.obj(
          "source-stp" -> stpToJson(criteria.getPath().getSourceSTP()),
          "destination-stp" -> stpToJson(criteria.getPath().getDestSTP()),
          "start-time" -> criteria.getSchedule().getStartTime().toString(),
          "end-time" -> criteria.getSchedule().getEndTime().toString(),
          "bandwidth" -> criteria.getBandwidth(),
          "reply-to" -> "http://localhost:9090/pce/reply",
          "correation-id" -> correlationId.toString,
          "algorithm" -> "chain"))
    }

    private def stpToJson(stp: StpType): JsObject =
      Json.obj("network-id" -> stp.getNetworkId(), "local-id" -> stp.getLocalId())
  }

  class DummyPceRequesterActor extends Actor {
    def receive = {
      case pce: PathComputationRequest =>
        sender !
          Inbound(PathComputationConfirmed(
            pce.correlationId,
            Seq(ComputedSegment(
              pce.criteria.getPath().getSourceSTP,
              pce.criteria.getPath().getDestSTP(),
              "urn:ogf:network:surnfnet.nl",
              new URL("http://localhost:9000/nsi-v2/ConnectionServiceProvider"),
              NoAuthentication))))
    }
  }

  private[controllers] def handleResponse(message: NsiRequesterOperation): Unit = atomic { implicit transaction =>
    continuations.get(message.correlationId) foreach { f =>
      f(message)
    }
  }
}
