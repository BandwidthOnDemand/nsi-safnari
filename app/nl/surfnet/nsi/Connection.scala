package nl.surfnet.nsi

import nl.surfnet.nsi.NsiAcknowledgement._
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType

case class Inbound(message: Message)
case class Outbound(message: Message)

case class PathComputationRequest(correlationId: CorrelationId) extends Request
case class PathComputationFailed(correlationId: CorrelationId) extends Response
case class PathComputationConfirmed(correlationId: CorrelationId) extends Response

trait Connection {
  def id: ConnectionId

  def handleAck: Message => (Connection, Seq[Message])
  def handleInbound(message: Message): (Connection, Message, Seq[Message])
  def handleOutbound: Message => (Connection, Seq[Message])

  def reservationState: ReservationState
}
case class NewConnection(id: ConnectionId) extends Connection {
  override def handleAck = {
    case message: ReserveResponse => (ExistingConnection(message.connectionId, message.headers), Seq(PathComputationRequest(newCorrelationId)))
    case _ => ???
  }
  override def handleInbound(message: Message) = message match {
    case message: NsiProviderOperation.Reserve => (this, ReserveResponse(message.headers.copy(replyTo = None), id), Seq.empty)
  }
  def handleOutbound = {
    case _ => ???
  }
  def reservationState: ReservationState = InitialReservationState
}
case class ExistingConnection(id: ConnectionId, headers: NsiHeaders, reservationState: ReservationState = InitialReservationState) extends Connection {

  override def handleInbound(message: Message) = message match {
    case message: PathComputationFailed                  => (this, 200, Seq(NsiRequesterOperation.ReserveFailed(headers, id)))
    case message: PathComputationConfirmed               => (this, 200, Seq(NsiProviderOperation.Reserve(headers))) // TODO generate new headers and multiple requests...
    case message: NsiRequesterOperation.ReserveConfirmed => (this, GenericAck(message.headers.copy(replyTo = None)), Seq(NsiRequesterOperation.ReserveConfirmed(headers.copy(replyTo = None)))) // TODO
  }

  override def handleAck = {
    case message: NsiAcknowledgement.ReserveResponse => (this, Seq.empty)
  }
  def handleOutbound = {
    case message: NsiRequesterOperation.ReserveFailed => (copy(reservationState = FailedReservationState), Seq.empty)
    case message: PathComputationRequest              => (this, Seq.empty)
    case message: NsiProviderOperation.Reserve        => (this, Seq.empty)
  }
}
