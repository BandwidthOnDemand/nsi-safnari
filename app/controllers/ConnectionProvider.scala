package controllers

import play.api.mvc.{ Request => _, Response => _, _ }
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import support.ExtraBodyParsers._
import models._
import java.util.UUID
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.WS
import nl.surfnet.nsi._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import scala.concurrent.stm._
import java.net.URI
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

object ConnectionProvider extends Controller with SoapWebService {
  implicit val actorSystem = ActorSystem("NSI")
  implicit val timeout = Timeout(2.seconds)
  private val state = TMap.empty[ConnectionId, ActorRef]
  private[controllers] val continuations = TMap.empty[CorrelationId, Response => Unit]

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionProvider.request().absoluteURL()

  private def replyToClient(replyTo: Option[URI])(response: Response): Unit = {
    replyTo.foreach { replyTo =>
      Future {
        blocking { Thread.sleep(3000) }
        WS.url(replyTo.toASCIIString()).post(response.asInstanceOf[NsiMessage])
      }
    }
  }

  def request = NsiEndPoint {
    case request: Request =>
      handleRequest(request)(replyToClient(request.replyTo))
    case query: NsiProviderOperation =>
      Future.successful(handleQuery(query)(replyToClient(query.replyTo)))
  }

  private def handleQuery(message: NsiProviderOperation)(replyTo: Response => Unit): NsiResponseMessage = message match {
    case q: NsiProviderOperation.QuerySummary =>
      ???
//      val connections = state.single.snapshot
//      val connectionStates = q.connectionIds.map(id => connections.get(id).map(connection => id -> FailedReservationState /*connection.reservationState*/)).flatten
//      replyTo(NsiRequesterOperation.QuerySummaryConfirmed(q.headers.copy(replyTo = None), connectionStates))
//      NsiResponseMessage.GenericAck(q.headers)
  }

  private[controllers] def handleResponse(message: Response): Unit = atomic { implicit transaction =>

    for {
      f <- continuations.get(message.correlationId)
    } {
      f(message)

    }
  }

  private val outboundActor = actorSystem.actorOf(Props(new Actor {
    def receive = {
      case pce: PathComputationRequest =>
        sender ! Inbound(PathComputationFailed(pce.correlationId))
      case request: Request =>
        val connection = sender
        continuations.single.put(request.correlationId, response => {
          connection ! Inbound(response)
        })
      case response: Response =>
        handleResponse(response)
    }
  }))

  private[controllers] def handleRequest(message: NsiProviderOperation with Request)(replyTo: Response => Unit): Future[NsiResponseMessage] = atomic { implicit transaction =>
    continuations(message.correlationId) = replyTo

    val connection = message.optionalConnectionId match {
      case None               =>
        val id = newConnectionId
        val c = actorSystem.actorOf(Props(new ConnectionActor(id, outboundActor)))
        state(id) = c
        c
      case Some(connectionId) => state.getOrElse(connectionId, throw new IllegalStateException("unknown connection id"))
    }

    connection ? Inbound(message) map (_.asInstanceOf[NsiResponseMessage])
  }
}
