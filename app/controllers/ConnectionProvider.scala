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

object ConnectionProvider extends Controller with SoapWebService {

  private val state = TMap.empty[ConnectionId, Connection]
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
      handleQuery(query)(replyToClient(query.replyTo))
  }

  private def handleQuery(message: NsiProviderOperation)(replyTo: Response => Unit): NsiResponseMessage = message match {
    case q: NsiProviderOperation.QuerySummary =>
      val connections = state.single.snapshot
      val connectionStates = q.connectionIds.map(id => connections.get(id).map(connection => id -> connection.reservationState)).flatten
      replyTo(NsiRequesterOperation.QuerySummaryConfirmed(q.headers.copy(replyTo = None), connectionStates))
      NsiResponseMessage.GenericAck(q.headers)
  }

  private[controllers] def handleResponse(message: Response): Unit = atomic { implicit transaction =>

    for {
      f <- continuations.get(message.correlationId)
    } {
      f(message)

    }
  }

  private[controllers] def handleRequest(message: NsiProviderOperation with Request)(replyTo: Response => Unit): NsiResponseMessage = atomic { implicit transaction =>
    continuations(message.correlationId) = replyTo

    val connection = message.optionalConnectionId match {
      case None               => NewConnection(newConnectionId)
      case Some(connectionId) => state.getOrElse(connectionId, throw new IllegalStateException("unknown connection id"))
    }

    val (connection2, Seq(ack: NsiResponseMessage)) = connection.handle(Inbound(message))

    val (updatedConnection, asyncOutboundMessages) = connection2.handle(Outbound(ack))
    state(updatedConnection.id) = updatedConnection
    handleAsyncOutboundMessages(updatedConnection.id, asyncOutboundMessages)

    ack
  }

  def handleAsyncOutboundMessages(connectionId: ConnectionId, messages: Seq[Message]): Unit = atomic { implicit txn =>
    messages.collect {
      case request: Request =>
        continuations(request.correlationId) = response => {
          val (updatedConnection, asyncOutboundMessages) = state(connectionId).handle(Inbound(response))
          state(connectionId) = updatedConnection
          handleAsyncOutboundMessages(connectionId, asyncOutboundMessages)
        }
      case response: Response =>
        handleResponse(response)
    }
  }

  //
  //    message match {
  //      case r: NsiProviderOperation.Reserve =>
  //        val connectionId = UUID.randomUUID.toString()
  //        state.single.transform(_ + (connectionId -> InitialReservationState))
  //        replyTo(ReserveFailed(r.headers.copy(replyTo = None), connectionId))
  //        NsiResponseMessage.ReserveResponse(r.headers, connectionId)
  //      case q: NsiProviderOperation.QuerySummary =>
  //        val connectionStates = state.single.getWith(state => q.connectionIds.map(id => state.get(id).map(id -> _))).flatten
  //        replyTo(NsiRequesterOperation.QuerySummaryConfirmed(q.headers.copy(replyTo = None), connectionStates))
  //        NsiResponseMessage.GenericAck(q.headers)
  //      case m =>
  //        NsiResponseMessage.ServiceException(m.headers)
  //    }
  //  }
}
