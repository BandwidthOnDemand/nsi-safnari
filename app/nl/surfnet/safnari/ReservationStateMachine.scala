package nl.surfnet.safnari

import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import scala.collection.JavaConverters._
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

sealed abstract class ReservationState(val jaxb: ReservationStateEnumType)
case object InitialReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object FailedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_FAILED)
case object ReservedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object PathComputationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object CheckingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object HeldReservationState extends ReservationState(ReservationStateEnumType.RESERVE_HELD)
case object CommittingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_COMMITTING)
case object CommitFailedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object AbortingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_ABORTING)
case object AbortedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object TimeoutReservationState extends ReservationState(ReservationStateEnumType.RESERVE_TIMEOUT)

case class ReservationStateMachineData(
  commandReplyHeaders: NsiHeaders,
  globalReservationId: Option[String],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  segments: Map[CorrelationId, ComputedSegment] = Map.empty,
  connections: Map[ConnectionId, CorrelationId] = Map.empty,
  childConnectionStates: Map[ConnectionId, ReservationState] = Map.empty,
  childExceptions: Map[ConnectionId, ServiceExceptionType] = Map.empty,
  childTimeouts: Map[ConnectionId, ReserveTimeoutRequestType] = Map.empty) {

  def awaitingConnectionId = segments.keySet -- connections.values

  def aggregatedReservationState: ReservationState =
    if (awaitingConnectionId.isEmpty && childConnectionStates.isEmpty) CheckingReservationState
    else if (awaitingConnectionId.nonEmpty || childConnectionStates.values.exists(_ == CheckingReservationState)) CheckingReservationState
    else if (childConnectionStates.values.exists(_ == FailedReservationState)) FailedReservationState
    else if (childConnectionStates.values.forall(_ == HeldReservationState)) HeldReservationState
    else if (childConnectionStates.values.exists(_ == TimeoutReservationState)) TimeoutReservationState
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

  def updateChild(connectionId: ConnectionId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None, childTimeout: Option[ReserveTimeoutRequestType] = None) =
    copy(
      childConnectionStates = childConnectionStates.updated(connectionId, reservationState),
      childExceptions = childException.map(childExceptions.updated(connectionId, _)).getOrElse(childExceptions - connectionId),
      childTimeouts = childTimeout.map(childTimeouts.updated(connectionId, _)).getOrElse(childTimeouts))

  def startProcessingNewCommand(commandHeaders: NsiHeaders, transitionalState: ReservationState) =
    copy(
      commandReplyHeaders = commandHeaders.asReply,
      childConnectionStates = childConnectionStates.map { _._1 -> transitionalState },
      childExceptions = Map.empty)

  def childHasState(connectionId: ConnectionId, state: ReservationState) =
    childConnectionStates.getOrElse(connectionId, CheckingReservationState) == state

  def children: Map[ConnectionId, ComputedSegment] = connections.map {
    case (connectionId, correlationId) => connectionId -> segments(correlationId)
  }
}

class ReservationStateMachine(
  id: ConnectionId,
  initialReserve: InitialReserve,
  pceReplyUri: URI,
  newCorrelationId: () => CorrelationId,
  newNsiHeaders: ProviderEndPoint => NsiHeaders,
  failed: NsiError => GenericFailedType)
  extends FiniteStateMachine[ReservationState, ReservationStateMachineData, InboundMessage, OutboundMessage](
    InitialReservationState,
    ReservationStateMachineData(
      initialReserve.headers.asReply,
      Option(initialReserve.body.getGlobalReservationId()),
      Option(initialReserve.body.getDescription()),
      initialReserve.criteria)) {

  when(InitialReservationState) {
    case Event(FromRequester(message: InitialReserve), _) =>
      goto(PathComputationState)
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data) =>
      val segments = message.segments.map(newCorrelationId() -> _)
      goto(CheckingReservationState) using data.copy(segments = segments.toMap)
    case Event(FromPce(message: PathComputationFailed), _) =>
      goto(FailedReservationState)
  }

  when(CheckingReservationState) {
    case Event(FromProvider(message: ReserveConfirmed), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedConnectionId(message.correlationId, message.connectionId)
        .updateChild(message.connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(message: ReserveFailed), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedConnectionId(message.correlationId, message.connectionId)
        .updateChild(message.connectionId, FailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(HeldReservationState) {
    case Event(FromRequester(message: ReserveCommit), data) =>
      val newData = data.startProcessingNewCommand(message.headers, CommittingReservationState)
      goto(CommittingReservationState) using newData
    case Event(FromRequester(message: ReserveAbort), data) =>
      val newData = data.startProcessingNewCommand(message.headers, AbortingReservationState)
      goto(AbortingReservationState) using newData
    case Event(FromProvider(message: ReserveTimeout), data) =>
      val newData = data.updateChild(connectionId = message.connectionId, reservationState = TimeoutReservationState, childTimeout = Some(message.timeout))
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommittingReservationState) {
    case Event(FromProvider(message: ReserveCommitConfirmed), data) if data.childHasState(message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(message.connectionId, ReservedReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(message: ReserveCommitFailed), data) if data.childHasState(message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(message.connectionId, CommitFailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(AbortingReservationState) {
    case Event(FromProvider(message: ReserveAbortConfirmed), data) if data.childHasState(message.connectionId, AbortingReservationState) =>
      val newData = data.updateChild(message.connectionId, AbortedReservationState)
      goto(newData.aggregatedReservationState)
  }

  when(TimeoutReservationState) {
    case Event(FromProvider(message: ReserveTimeout), data) =>
      val newData = data.updateChild(connectionId = message.connectionId, reservationState = TimeoutReservationState, childTimeout = Some(message.timeout))
      stay using newData
    case Event(FromRequester(message: ReserveAbort), data) =>
      val newData = data.startProcessingNewCommand(message.headers, AbortingReservationState)
      goto(AbortingReservationState) using newData
  }

  when(ReservedReservationState)(PartialFunction.empty)
  when(FailedReservationState)(PartialFunction.empty)
  when(CommitFailedReservationState)(PartialFunction.empty)

  onTransition {
    case InitialReservationState -> PathComputationState =>
      Seq(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, initialReserve.criteria.getSchedule(), initialReserve.service)))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData
      data.segments.map {
        case (correlationId, segment) =>
          val service = segment.service
          val criteria = new ReservationRequestCriteriaType().
            withPointToPointService(service).
            withSchedule(data.criteria.getSchedule()).
            withVersion(data.criteria.getVersion())
          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria)

          ToProvider(InitialReserve(newNsiHeaders(segment.provider).copy(correlationId = correlationId), reserveType, Conversion.invert(criteria).right.get, service), segment.provider)
      }.toVector
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(_, failed(NsiError.PathComputationNoPath)))
    case CheckingReservationState -> FailedReservationState =>
      respond(ReserveFailed(_, failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava))))
    case CheckingReservationState -> HeldReservationState =>
      respond(ReserveConfirmed(_, id, nextStateData.criteria))
    case HeldReservationState -> CommittingReservationState =>
      nextStateData.connections.map {
        case (connectionId, correlationId) =>
          val seg = nextStateData.segments(correlationId)
          ToProvider(ReserveCommit(newNsiHeaders(seg.provider), connectionId), seg.provider)
      }.toVector
    case HeldReservationState -> AbortingReservationState =>
      nextStateData.connections.map {
        case (connectionId, correlationId) =>
          val seg = nextStateData.segments(correlationId)
          ToProvider(ReserveAbort(newNsiHeaders(seg.provider), connectionId), seg.provider)
      }.toVector
    case HeldReservationState -> TimeoutReservationState =>
      val (timedOutConnectionId, timeout) = nextStateData.childTimeouts.head
      respond(ReserveTimeout(_, timeout.withConnectionId(id)))
//          new ReserveTimeoutRequestType()
//        .withConnectionId(id)
//        .withTimeStamp(timeout.getTimeStamp())
//        .withTimeoutValue(timeout.getTimeoutValue())
//        .withOriginatingConnectionId(timedOutConnectionId)
//        .withOriginatingNSA(timeout.getOriginatingNSA()))
    case CommittingReservationState -> ReservedReservationState =>
      respond(ReserveCommitConfirmed(_, id))
    case CommittingReservationState -> CommitFailedReservationState =>
      respond(ReserveCommitFailed(_, failed(NsiError.InternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava))))
    case ReservedReservationState -> CheckingReservationState => ???
    case FailedReservationState -> AbortingReservationState   => ???
    case AbortingReservationState -> ReservedReservationState => ???
  }

  def segmentKnown(connectionId: ConnectionId) = stateData.childConnectionStates.contains(connectionId)
  def childConnectionState(connectionId: ConnectionId): ReservationStateEnumType = stateData.childConnectionStates(connectionId).jaxb
  def childConnections: Map[ConnectionId, ComputedSegment] = stateData.children
  def reservationState = stateName.jaxb
  def criteria = stateData.criteria
  def version = stateData.criteria.getVersion()

  private def respond(f: NsiHeaders => NsiRequesterOperation) = Seq(ToRequester(f(nextStateData.commandReplyHeaders)))
}
