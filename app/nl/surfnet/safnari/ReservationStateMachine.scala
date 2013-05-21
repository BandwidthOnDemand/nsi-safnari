package nl.surfnet.safnari

import com.twitter.bijection.Injection
import java.net.URI
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
import scala.collection.JavaConverters._

sealed abstract class ReservationState(val jaxb: ReservationStateEnumType)
case object InitialReservationState extends ReservationState(ReservationStateEnumType.INITIAL)
case object FailedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_FAILED)
case object ReservedReservationState extends ReservationState(ReservationStateEnumType.RESERVED)
case object PathComputationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object CheckingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object HeldReservationState extends ReservationState(ReservationStateEnumType.RESERVE_HELD)
case object CommittingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_COMMITTING)
case object CommitFailedReservationState extends ReservationState(ReservationStateEnumType.RESERVED)
case object AbortingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_ABORTING)
case object AbortedReservationState extends ReservationState(ReservationStateEnumType.RESERVED)
case object TimeoutReservationState extends ReservationState(ReservationStateEnumType.RESERVE_TIMEOUT)

case class ReservationStateMachineData(
  commandCorrelationId: CorrelationId,
  globalReservationId: Option[String],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  segments: Map[CorrelationId, ComputedSegment] = Map.empty,
  connections: Map[ConnectionId, CorrelationId] = Map.empty,
  childConnectionStates: Map[ConnectionId, ReservationState] = Map.empty,
  childExceptions: Map[ConnectionId, ServiceExceptionType] = Map.empty) {

  def awaitingConnectionId = segments.keySet -- connections.values

  def aggregatedReservationState: ReservationState =
    if (awaitingConnectionId.isEmpty && childConnectionStates.isEmpty) CheckingReservationState
    else if (awaitingConnectionId.nonEmpty || childConnectionStates.values.exists(_ == CheckingReservationState)) CheckingReservationState
    else if (childConnectionStates.values.exists(_ == FailedReservationState)) FailedReservationState
    else if (childConnectionStates.values.forall(_ == HeldReservationState)) HeldReservationState
    else if (childConnectionStates.values.exists(_ == CommittingReservationState)) CommittingReservationState
    else if (childConnectionStates.values.exists(_ == CommitFailedReservationState)) CommitFailedReservationState
    else if (childConnectionStates.values.forall(_ == ReservedReservationState)) ReservedReservationState
    else if (childConnectionStates.values.exists(_ == AbortingReservationState)) AbortingReservationState
    else if (childConnectionStates.values.forall(_ == AbortedReservationState)) AbortedReservationState
    else if (childConnectionStates.values.exists(_ == TimeoutReservationState)) TimeoutReservationState
    else throw new IllegalStateException(s"cannot determine aggregated state from ${childConnectionStates.values.mkString(",")}")

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId) = {
    require(awaitingConnectionId.contains(correlationId), s"bad correlationId: $correlationId, awaiting $awaitingConnectionId")
    require(!childConnectionStates.contains(connectionId), s"duplicate connectionId: $connectionId, already have $childConnectionStates")
    copy(connections = connections.updated(connectionId, correlationId))
  }

  def updateChild(connectionId: ConnectionId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None) =
    copy(
      childConnectionStates = childConnectionStates.updated(connectionId, reservationState),
      childExceptions = childException.map(childExceptions.updated(connectionId, _)).getOrElse(childExceptions - connectionId))

  def startProcessingNewCommand(commandCorrelationId: CorrelationId, transitionalState: ReservationState) =
    copy(
      commandCorrelationId = commandCorrelationId,
      childConnectionStates = childConnectionStates.map { _._1 -> transitionalState },
      childExceptions = Map.empty)

  def childHasState(connectionId: ConnectionId, state: ReservationState) =
    childConnectionStates.getOrElse(connectionId, CheckingReservationState) == state

  def children = connections.map {
    case (connectionId, correlationId) =>
      connectionId -> segments(correlationId).provider
  }
}

class ReservationStateMachine(
  id: ConnectionId, initialReserve: Reserve, pceReplyUri: URI,
  newCorrelationId: () => CorrelationId,
  outbound: Message => Unit,
  reservationHeld: ReservationStateMachineData => Unit,
  failed: NsiError => GenericFailedType)
  extends FiniteStateMachine[ReservationState, ReservationStateMachineData](
    InitialReservationState,
    ReservationStateMachineData(
      initialReserve.correlationId,
      Option(initialReserve.body.getGlobalReservationId()),
      Option(initialReserve.body.getDescription()),
      Injection.invert(initialReserve.body.getCriteria()).getOrElse(sys.error("Bad initial reservation criteria")))) {

  when(InitialReservationState) {
    case Event(FromRequester(message: Reserve), _) =>
      goto(PathComputationState) replying ReserveResponse(message.correlationId, id)
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data) =>
      val segments = message.segments.map(newCorrelationId() -> _)
      goto(CheckingReservationState) using data.copy(segments = segments.toMap) replying 200
    case Event(FromPce(message: PathComputationFailed), _) =>
      goto(FailedReservationState) replying 200
  }

  when(CheckingReservationState) {
    case Event(FromProvider(message: ReserveConfirmed), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedConnectionId(message.correlationId, message.connectionId)
        .updateChild(message.connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
    case Event(FromProvider(message: ReserveFailed), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedConnectionId(message.correlationId, message.connectionId)
        .updateChild(message.connectionId, FailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
  }

  when(HeldReservationState) {
    case Event(FromRequester(message: ReserveCommit), data) =>
      val newData = data.startProcessingNewCommand(message.correlationId, CommittingReservationState)
      goto(CommittingReservationState) using newData replying GenericAck(message.correlationId)
    case Event(FromRequester(message: ReserveAbort), data) =>
      val newData = data.startProcessingNewCommand(message.correlationId, AbortingReservationState)
      goto(AbortingReservationState) using newData replying GenericAck(message.correlationId)
  }

  when(CommittingReservationState) {
    case Event(FromProvider(message: ReserveCommitConfirmed), data) if data.childHasState(message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(message.connectionId, ReservedReservationState)
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
    case Event(FromProvider(message: ReserveCommitFailed), data) if data.childHasState(message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(message.connectionId, CommitFailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
  }

  when(AbortingReservationState) {
    case Event(FromProvider(message: ReserveAbortConfirmed), data) if data.childHasState(message.connectionId, AbortingReservationState) =>
      val newData = data.updateChild(message.connectionId, AbortedReservationState)
      goto(newData.aggregatedReservationState) replying GenericAck(message.correlationId)
  }

  when(ReservedReservationState)(PartialFunction.empty)
  when(FailedReservationState)(PartialFunction.empty)
  when(CommitFailedReservationState)(PartialFunction.empty)
  when(AbortedReservationState)(PartialFunction.empty)

  onTransition {
    case InitialReservationState -> PathComputationState =>
      outbound(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, nextStateData.criteria)))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData
      data.segments.foreach {
        case (correlationId, segment) =>
          val criteria = new ReservationRequestCriteriaType().
            withBandwidth(data.criteria.getBandwidth()).
            withPath(new PathType().withSourceSTP(segment.sourceStp).withDestSTP(segment.destinationStp).withDirectionality(DirectionalityType.BIDIRECTIONAL)).
            withSchedule(data.criteria.getSchedule()).
            withServiceAttributes(data.criteria.getServiceAttributes()).
            withVersion(data.criteria.getVersion())

          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria)

          outbound(ToProvider(Reserve(correlationId, reserveType), segment.provider))
      }
    case PathComputationState -> FailedReservationState =>
      outbound(ToRequester(ReserveFailed(nextStateData.commandCorrelationId, failed(NsiError.PathComputationNoPath))))
    case CheckingReservationState -> FailedReservationState =>
      outbound(ToRequester(ReserveFailed(nextStateData.commandCorrelationId, failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava)))))
    case CheckingReservationState -> HeldReservationState =>
      reservationHeld(nextStateData)
      outbound(ToRequester(ReserveConfirmed(nextStateData.commandCorrelationId, id, nextStateData.criteria)))
    case HeldReservationState -> CommittingReservationState =>
      nextStateData.connections.foreach {
        case (connectionId, correlationId) =>
          val seg = nextStateData.segments(correlationId)
          outbound(ToProvider(ReserveCommit(newCorrelationId(), connectionId), seg.provider))
      }
    case HeldReservationState -> AbortingReservationState =>
      nextStateData.connections.foreach {
        case (connectionId, correlationId) =>
          val seg = nextStateData.segments(correlationId)
          outbound(ToProvider(ReserveAbort(newCorrelationId(), connectionId), seg.provider))
      }
    case CommittingReservationState -> ReservedReservationState =>
      outbound(ToRequester(ReserveCommitConfirmed(nextStateData.commandCorrelationId, id)))
    case CommittingReservationState -> CommitFailedReservationState =>
      outbound(ToRequester(ReserveCommitFailed(nextStateData.commandCorrelationId, failed(NsiError.InternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava)))))
    case ReservedReservationState -> CheckingReservationState => ???
    case FailedReservationState -> AbortingReservationState   => ???
    case AbortingReservationState -> ReservedReservationState => ???
  }

  def segmentKnown(connectionId: ConnectionId) = stateData.childConnectionStates.contains(connectionId)
  def segments = stateData.childConnectionStates
  def reservationState = new ReservationStateType().withState(stateName.jaxb)
  def criteria = stateData.criteria
  def version = stateData.criteria.getVersion()
}
