package nl.surfnet.nsi

import akka.actor._
import com.twitter.bijection.Injection
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import org.ogf.schemas.nsi._2013._04.connection.types._

case class Inbound(message: Message)
case class Outbound(message: Message)

class ConnectionActor(id: ConnectionId, newCorrelationId: () => CorrelationId, outbound: ActorRef) extends Actor with FSM[ReservationState, Connection] {

  startWith(InitialReservationState, NewConnection(id))

  when(InitialReservationState) {
    case Event(Inbound(message: Reserve), _) =>
      val criteria = Injection.invert(message.body.getCriteria())
      goto(CheckingReservationState) using ExistingConnection(
        id = id,
        reserveCorrelationId = message.correlationId,
        globalReservationId = Option(message.body.getGlobalReservationId()),
        description = Option(message.body.getDescription()),
        criteria = criteria.getOrElse(sys.error("Bad initial reservation criteria"))) replying ReserveResponse(message.correlationId, id)
  }

  when(CheckingReservationState) {
    case Event(Inbound(message: PathComputationConfirmed), data: ExistingConnection) =>
      val segments = message.segments.map { seg =>
        newCorrelationId() -> new ReserveType().
          withGlobalReservationId(data.globalReservationId.orNull).
          withDescription(data.description.orNull).
          withCriteria(Injection.apply(seg))
      }
      segments.foreach { case (correlationId, reserveType) => outbound ! Reserve(correlationId, reserveType) }

      val newData = data.copy(awaitingConnectionId = segments.map(_._1).toSet, segments = message.segments)
      goto(newData.aggregatedReservationState) using newData replying 200
    case Event(Inbound(message: PathComputationFailed), _) =>
      goto(FailedReservationState) replying 200

    case Event(Inbound(message: ReserveConfirmed), data: ExistingConnection) =>
      val newData = data.receivedConnectionId(message.correlationId, message.connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
    case Event(Inbound(message: ReserveFailed), data: ExistingConnection) =>
      val newData = data.receivedConnectionId(message.correlationId, message.connectionId, FailedReservationState)
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
  }

  when(HeldReservationState) {
    case Event(Inbound(commit: ReserveCommit), data: ExistingConnection) =>
      val newData = data.copy(reserveCorrelationId = commit.correlationId, downstreamConnections = data.downstreamConnections.map { _.copy(_2 = CommittingReservationState) })
      goto(CommittingReservationState) using newData replying GenericAck(commit.correlationId)
    case Event(Inbound(abort: ReserveAbort), data: ExistingConnection) =>
      val newData = data.copy(reserveCorrelationId = abort.correlationId, downstreamConnections = data.downstreamConnections.map { _.copy(_2 = AbortingReservationState) })
      goto(AbortingReservationState) using newData replying GenericAck(abort.correlationId)
  }

  when(CommittingReservationState) {
    case Event(Inbound(message: ReserveCommitConfirmed), data: ExistingConnection) =>
      val newData = data.copy(downstreamConnections = data.downstreamConnections + (message.connectionId -> ReservedReservationState))
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
    case Event(Inbound(message: ReserveCommitFailed), data: ExistingConnection) =>
      val newData = data.copy(downstreamConnections = data.downstreamConnections + (message.connectionId -> CommittingReservationState /* TODO really? */ ))
      stay replying GenericAck(message.correlationId)
  }

  when(AbortingReservationState) {
    case Event(Inbound(confirmed: ReserveAbortConfirmed), _) => stay replying GenericAck(confirmed.correlationId)
  }

  when(FailedReservationState)(FSM.NullFunction)
  when(ReservedReservationState)(FSM.NullFunction)

  whenUnhandled {
    case Event('query, data: ExistingConnection) =>
      stay replying (new QuerySummaryResultType().
        withGlobalReservationId(data.globalReservationId.orNull).
        withDescription(data.description.orNull).
        withConnectionId(data.id).
        withCriteria(data.criteria).
        withRequesterNSA("TODO" /*TODO*/).
        withConnectionStates(
          new ConnectionStatesType().
            withReservationState(new ReservationStateType().withVersion(data.criteria.getVersion()).withState(stateName.jaxb)).
            withProvisionState(new ProvisionStateType().withVersion(data.criteria.getVersion()).withState(ProvisionStateEnumType.UNKNOWN /*TODO*/)).
            withLifecycleState(new LifecycleStateType().withVersion(data.criteria.getVersion()).withState(LifecycleStateEnumType.INITIAL /*TODO*/)).
            withDataPlaneStatus(new DataPlaneStatusType().withVersion(data.criteria.getVersion()).withActive(false /*TODO*/))).
        withChildren(null /*TODO*/ ))
  }

  onTransition {
    case InitialReservationState -> CheckingReservationState => outbound ! PathComputationRequest(newCorrelationId(), nextStateData.asInstanceOf[ExistingConnection].criteria)
    case CheckingReservationState -> FailedReservationState  => outbound ! ReserveFailed(nextStateData.asInstanceOf[ExistingConnection].reserveCorrelationId, id)
    case CheckingReservationState -> HeldReservationState    => outbound ! ReserveConfirmed(nextStateData.asInstanceOf[ExistingConnection].reserveCorrelationId, id)
    case HeldReservationState -> CommittingReservationState =>
      nextStateData.asInstanceOf[ExistingConnection].downstreamConnections foreach {
        case (connectionId, _) =>
          outbound ! ReserveCommit(newCorrelationId(), connectionId)
      }
    case HeldReservationState -> AbortingReservationState =>
      nextStateData.asInstanceOf[ExistingConnection].downstreamConnections foreach {
        case (connectionId, _) =>
          outbound ! ReserveAbort(newCorrelationId(), connectionId)
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
case class ExistingConnection(
  id: ConnectionId,
  reserveCorrelationId: CorrelationId,
  globalReservationId: Option[String],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  segments: Seq[ReservationConfirmCriteriaType] = Seq.empty,
  awaitingConnectionId: Set[CorrelationId] = Set.empty,
  downstreamConnections: Map[ConnectionId, ReservationState] = Map.empty) extends Connection {

  def aggregatedReservationState: ReservationState =
    if (awaitingConnectionId.isEmpty && downstreamConnections.isEmpty) CheckingReservationState
    else if (awaitingConnectionId.nonEmpty || downstreamConnections.values.exists(_ == CheckingReservationState)) CheckingReservationState
    else if (downstreamConnections.values.exists(_ == FailedReservationState)) FailedReservationState
    else if (downstreamConnections.values.forall(_ == HeldReservationState)) HeldReservationState
    else if (downstreamConnections.values.exists(_ == CommittingReservationState)) CommittingReservationState
    else if (downstreamConnections.values.exists(_ == AbortingReservationState)) CommittingReservationState /* FIXME really? */
    else if (downstreamConnections.values.forall(_ == ReservedReservationState)) ReservedReservationState
    else ???

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId, reservationState: ReservationState): ExistingConnection = {
    require(awaitingConnectionId.contains(correlationId), s"bad correlationId: $correlationId, awaiting $awaitingConnectionId")
    require(!downstreamConnections.contains(connectionId), s"duplicate connectionId: $connectionId, already have $downstreamConnections")
    copy(
      awaitingConnectionId = awaitingConnectionId - correlationId,
      downstreamConnections = downstreamConnections + (connectionId -> reservationState))
  }
}
