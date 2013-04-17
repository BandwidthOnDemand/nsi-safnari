package nl.surfnet.nsi

import nl.surfnet.nsi.NsiResponseMessage.ReserveResponse
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType

case class Inbound(message: Message)
case class Outbound(message: Message)

case class PathComputationRequest(correlationId: CorrelationId) extends Request
case class PathComputationFailed(correlationId: CorrelationId) extends Response
case class PathComputationConfirmed(correlationId: CorrelationId) extends Response

trait Connection {
  def id: ConnectionId

  def handle: Message => (Connection, Seq[Message])

  def reservationState: ReservationState
}
case class NewConnection(id: ConnectionId) extends Connection {
  def handle: Message => (Connection, Seq[Message]) = {
    case Inbound(message: NsiProviderOperation.Reserve) => (this, Seq(ReserveResponse(message.headers.copy(replyTo = None), id)))
    case Outbound(message: ReserveResponse)             => (ExistingConnection(message.connectionId, message.headers), Seq(PathComputationRequest(newCorrelationId)))
  }
  def reservationState: ReservationState = InitialReservationState
}
case class ExistingConnection(id: ConnectionId, headers: NsiHeaders, reservationState: ReservationState = InitialReservationState) extends Connection {
  def handle: Message => (Connection, Seq[Message]) = {
    case Outbound(message: NsiRequesterOperation.ReserveFailed)   => (copy(reservationState = FailedReservationState), Seq.empty)
    case Outbound(message: PathComputationRequest)                => (this, Seq.empty)
    case Inbound(message: PathComputationFailed)                  => (this, Seq(NsiRequesterOperation.ReserveFailed(headers, id)))
    case Inbound(message: PathComputationConfirmed)               => (this, Seq(NsiProviderOperation.Reserve(headers)))
    case Outbound(message: NsiProviderOperation.Reserve)          => (this, Seq.empty)
    case Inbound(message: NsiResponseMessage.ReserveResponse)     => (this, Seq.empty)
    case Inbound(message: NsiRequesterOperation.ReserveConfirmed) => (this, Seq(NsiRequesterOperation.ReserveConfirmed()))
  }
}
