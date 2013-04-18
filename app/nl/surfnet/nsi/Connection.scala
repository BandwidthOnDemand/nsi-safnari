package nl.surfnet.nsi

import nl.surfnet.nsi.NsiResponseMessage._
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import akka.actor.FSM
import akka.actor._
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._


case class Inbound(message: Message)
case class Outbound(message: Message)

case class PathComputationRequest(correlationId: CorrelationId) extends Request
case class PathComputationFailed(correlationId: CorrelationId) extends Response
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[String]) extends Response

class ConnectionActor(id: ConnectionId, outbound: ActorRef) extends Actor with FSM[ReservationState, Connection] {

  startWith(InitialReservationState, NewConnection(id))

  when(InitialReservationState) {
    case Event(Inbound(message: Reserve), _) =>
      goto(CheckingReservationState) using ExistingConnection(id, message.headers) replying ReserveResponse(message.headers.copy(replyTo = None), id)
  }

  when(CheckingReservationState) {
    case Event(Inbound(message: PathComputationConfirmed), data: ExistingConnection) => stay using data.copy(outstandingReserveCount = message.segments.size)
    case Event(Inbound(message: PathComputationFailed), _) => goto(FailedReservationState) replying 200

    case Event(Inbound(message: ReserveConfirmed), data: ExistingConnection) =>
      val newData = data.copy(outstandingReserveCount = data.outstandingReserveCount - 1)
      goto(if (newData.outstandingReserveCount == 0) ReservedReservationState else stateName) using newData replying GenericAck(message.headers.asReply)
    case Event(Inbound(message: ReserveFailed), data: ExistingConnection) =>
      val newData = data.copy(outstandingReserveCount = data.outstandingReserveCount - 1)
      goto(if (newData.outstandingReserveCount == 0) FailedReservationState else stateName) using newData replying GenericAck(message.headers.asReply)
  }

  when(FailedReservationState)(FSM.NullFunction)
  when(ReservedReservationState)(FSM.NullFunction)

  whenUnhandled {
    case Event(Inbound(ack : ReserveResponse), _) => stay
  }

  onTransition {
    case InitialReservationState -> CheckingReservationState => outbound ! PathComputationRequest(newCorrelationId)
    case CheckingReservationState -> FailedReservationState => outbound ! ReserveFailed(nextStateData.asInstanceOf[ExistingConnection].headers.asReply, id)
    case CheckingReservationState -> ReservedReservationState => outbound ! ReserveConfirmed(nextStateData.asInstanceOf[ExistingConnection].headers.asReply)
  }
}

sealed trait Connection {
  def id: ConnectionId
}
case class NewConnection(id: ConnectionId) extends Connection
case class ExistingConnection(id: ConnectionId, headers: NsiHeaders, outstandingReserveCount: Int = 0) extends Connection
