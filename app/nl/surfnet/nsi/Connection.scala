package nl.surfnet.nsi

import nl.surfnet.nsi.NsiResponseMessage._
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import akka.actor.FSM
import akka.actor._
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._

case class Inbound(message: Message)
case class Outbound(message: Message)

class ConnectionActor(id: ConnectionId, outbound: ActorRef) extends Actor with FSM[ReservationState, Connection] {

  startWith(InitialReservationState, NewConnection(id))

  when(InitialReservationState) {
    case Event(Inbound(message: Reserve), _) =>
      goto(CheckingReservationState) using ExistingConnection(id, message.correlationId) replying ReserveResponse(message.correlationId, id)
  }

  when(CheckingReservationState) {
    case Event(Inbound(message: PathComputationConfirmed), data: ExistingConnection) =>
      (1 to message.segments.size).foreach { seg =>
        outbound ! Reserve(newCorrelationId)
      }
      stay using data.copy(outstandingResponses = message.segments.size, segments = message.segments) replying 200
    case Event(Inbound(message: PathComputationFailed), _) =>
      goto(FailedReservationState) replying 200

    case Event(Inbound(message: ReserveConfirmed), data: ExistingConnection) =>
      val newData = data.copy(outstandingResponses = data.outstandingResponses - 1)
      goto(if (newData.outstandingResponses == 0) HeldReservationState else stateName) using newData replying GenericAck(message.correlationId)
    case Event(Inbound(message: ReserveFailed), data: ExistingConnection) =>
      val newData = data.copy(outstandingResponses = data.outstandingResponses - 1)
      goto(if (newData.outstandingResponses == 0) FailedReservationState else stateName) using newData replying GenericAck(message.correlationId)
  }

  when(HeldReservationState) {
    case Event(Inbound(commit: ReserveCommit), data: ExistingConnection) =>
      goto(CommittingReservationState) using data.copy(reserveCorrelationId = commit.correlationId, outstandingResponses = data.segments.size) replying GenericAck(commit.correlationId)
    case Event(Inbound(abort: ReserveAbort), data: ExistingConnection) =>
      goto(AbortingReservationState) using data.copy(outstandingResponses = data.segments.size) replying GenericAck(abort.correlationId)
  }

  when(CommittingReservationState) {
    case Event(Inbound(message: ReserveCommitConfirmed), data: ExistingConnection) =>
      val newData = data.copy(outstandingResponses = data.outstandingResponses - 1)
      goto(if (newData.outstandingResponses == 0) ReservedReservationState else stateName) using newData replying GenericAck(message.correlationId)
    case Event(Inbound(message: ReserveCommitFailed), _) => stay replying GenericAck(message.correlationId)
  }

  when(AbortingReservationState) {
    case Event(Inbound(confirmed: ReserveAbortConfirmed), _) => stay replying GenericAck(confirmed.correlationId)
  }

  when(FailedReservationState)(FSM.NullFunction)
  when(ReservedReservationState)(FSM.NullFunction)

  whenUnhandled {
    case Event(Inbound(ack: ReserveResponse), _) => stay
  }

  onTransition {
    case InitialReservationState -> CheckingReservationState => outbound ! PathComputationRequest(newCorrelationId)
    case CheckingReservationState -> FailedReservationState  => outbound ! ReserveFailed(nextStateData.asInstanceOf[ExistingConnection].reserveCorrelationId, id)
    case CheckingReservationState -> HeldReservationState    => outbound ! ReserveConfirmed(nextStateData.asInstanceOf[ExistingConnection].reserveCorrelationId, id)
    case HeldReservationState -> CommittingReservationState =>
      nextStateData.asInstanceOf[ExistingConnection].segments foreach { seg =>
        outbound ! ReserveCommit(newCorrelationId, seg)
      }
    case HeldReservationState -> AbortingReservationState =>
      nextStateData.asInstanceOf[ExistingConnection].segments foreach { seg =>
        outbound ! ReserveAbort(newCorrelationId, seg)
      }
    case CommittingReservationState -> ReservedReservationState => outbound ! ReserveCommitConfirmed(nextStateData.asInstanceOf[ExistingConnection].reserveCorrelationId, id)
    case ReservedReservationState -> CheckingReservationState   => ???
    case FailedReservationState -> AbortingReservationState     => ???
    case AbortingReservationState -> ReservedReservationState   => ???
  }
}

sealed trait Connection {
  def id: ConnectionId
}
case class NewConnection(id: ConnectionId) extends Connection
case class ExistingConnection(id: ConnectionId, reserveCorrelationId: CorrelationId, segments: Seq[String] = Nil, outstandingResponses: Int = 0) extends Connection
