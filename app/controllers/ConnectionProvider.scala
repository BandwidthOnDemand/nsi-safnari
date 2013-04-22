package controllers

import java.util.UUID
import java.net.URI
import scala.concurrent.stm._
import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc.{ Request => _, Response => _, _ }
import play.api.libs.concurrent.Akka
import play.api.libs.ws.WS
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

object ConnectionProvider extends Controller with SoapWebService {
  implicit val timeout = Timeout(2.seconds)

  private val state = TMap.empty[ConnectionId, ActorRef]
  private[controllers] val continuations = TMap.empty[CorrelationId, NsiRequesterOperation => Unit]

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionProvider.request().absoluteURL()

  private def replyToClient(replyTo: Option[URI])(response: NsiRequesterOperation): Unit = {
    replyTo.foreach { replyTo =>
      Future {
        blocking { Thread.sleep(3000) }
        WS.url(replyTo.toASCIIString()).post(response.asInstanceOf[NsiMessage])
      }
    }
  }

  def request = NsiEndPoint {
    case query: NsiQuery =>
      Future.successful(handleQuery(query)(replyToClient(query.replyTo)))
    case request: NsiProviderOperation =>
      handleRequest(request)(replyToClient(request.replyTo))
  }

  private def handleQuery(message: NsiQuery)(replyTo: NsiRequesterOperation => Unit): NsiResponseMessage = message match {
    case q: NsiProviderOperation.QuerySummary =>
//      val connections = state.single.snapshot
//      val connectionStates = q.connectionIds.map { id =>
//        connections.get(id).map(_ ? 'queryState)
//      }
    // replyTo(NsiRequesterOperation.QuerySummaryConfirmed(q.headers.copy(replyTo = None), connectionStates))
    NsiResponseMessage.GenericAck(q.headers)
    case q => ???
  }

  private[controllers] def handleRequest(message: NsiProviderOperation)(replyTo: NsiRequesterOperation => Unit): Future[NsiResponseMessage] = atomic { implicit transaction =>
    continuations(message.correlationId) = replyTo

    val connection = message.optionalConnectionId match {
      case None =>
        val id = newConnectionId
        val c = Akka.system.actorOf(Props(new ConnectionActor(id, outboundActor)))
        state(id) = c
        c
      case Some(connectionId) =>
        state.getOrElse(connectionId, throw new IllegalStateException("Unknown connection id"))
    }

    connection ? Inbound(message) map (_.asInstanceOf[NsiResponseMessage])
  }

  private val outboundActor = Akka.system.actorOf(Props(new Actor {
    def receive = {
      case pce: PathComputationRequest =>
        sender ! Inbound(PathComputationConfirmed(pce.correlationId, Seq("1")))
      case reserve: Reserve =>
        sender ! Inbound(ReserveConfirmed(reserve.headers.asReply, newConnectionId))
      case commit: ReserveCommit =>
        sender ! Inbound(ReserveCommitConfirmed(commit.headers.asReply, commit.connectionId))
      case response: NsiRequesterOperation =>
        handleResponse(response)
    }
  }))

  private[controllers] def handleResponse(message: NsiRequesterOperation): Unit = atomic { implicit transaction =>
    continuations.get(message.correlationId) foreach { f =>
      f(message)
    }
  }

}
