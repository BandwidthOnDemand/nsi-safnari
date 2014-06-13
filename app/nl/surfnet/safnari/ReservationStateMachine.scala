package nl.surfnet.safnari

import java.net.URI
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import scala.collection.JavaConverters._

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
  currentCommand: NsiProviderMessage[NsiProviderOperation],
  globalReservationId: Option[GlobalReservationId],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  awaitingReserveReply: Set[CorrelationId] = Set.empty,
  childConnectionStates: Map[CorrelationId, ReservationState] = Map.empty,
  childExceptions: Map[CorrelationId, ServiceExceptionType] = Map.empty,
  childTimeouts: Map[CorrelationId, ReserveTimeoutRequestType] = Map.empty,
  pceError: Option[NsiError] = None) {

  def receivedSegments(segments: Seq[(CorrelationId, ComputedSegment)]) =
    copy(
      awaitingReserveReply = segments.map(_._1).toSet)

  def aggregatedReservationState: ReservationState =
    if (awaitingReserveReply.isEmpty && childConnectionStates.isEmpty) CheckingReservationState
    else if (awaitingReserveReply.nonEmpty || childConnectionStates.values.exists(_ == CheckingReservationState)) CheckingReservationState
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

  def receivedReserveReply(correlationId: CorrelationId) = copy(awaitingReserveReply = awaitingReserveReply - correlationId)

  def updateChild(initialCorrelationId: CorrelationId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None, childTimeout: Option[ReserveTimeoutRequestType] = None): ReservationStateMachineData  = {
    updateChildStatus(initialCorrelationId, reservationState, childException, childTimeout)
  }
  def updateChildStatus(correlationId: CorrelationId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None, childTimeout: Option[ReserveTimeoutRequestType] = None): ReservationStateMachineData =
    copy(
      childConnectionStates = childConnectionStates.updated(correlationId, reservationState),
      childExceptions = childException.fold(childExceptions - correlationId)(exception => childExceptions.updated(correlationId, exception)),
      childTimeouts = childTimeout.fold(childTimeouts)(timeout => childTimeouts.updated(correlationId, timeout)))

  def startProcessingNewCommand(command: NsiProviderMessage[NsiProviderOperation], transitionalState: ReservationState, children: ChildConnectionIds) = {
    // Skip aborted state when we never received a child connection id due to an immediate service exception.
    val stateForChildConnectionsWithoutConnectionId = if (transitionalState == AbortingReservationState) AbortedReservationState else transitionalState
    copy(
      currentCommand = command,
      childConnectionStates = childConnectionStates.map {
        case (correlationId, _) =>
          correlationId -> (if (children hasConnectionId correlationId) transitionalState else stateForChildConnectionsWithoutConnectionId)
      },
      childExceptions = Map.empty)
    }

  def childHasState(correlationId: CorrelationId, state: ReservationState): Boolean = {
    val currentState = childConnectionStates.get(correlationId)
    state == currentState.getOrElse(CheckingReservationState)
  }

  def childHasState(connectionId: ConnectionId, state: ReservationState, children: ChildConnectionIds): Boolean = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"missing child connection id for $connectionId"))
    childHasState(correlationId, state)
  }
}

class ReservationStateMachine(
  id: ConnectionId,
  initialReserve: NsiProviderMessage[InitialReserve],
  pceReplyUri: URI,
  children: => ChildConnectionIds,
  newCorrelationId: () => CorrelationId,
  newNsiHeaders: ProviderEndPoint => NsiHeaders,
  newInitialReserveNsiHeaders: ProviderEndPoint => NsiHeaders,
  newNotificationId: () => Int,
  pathComputationAlgorithm: PathComputationAlgorithm,
  failed: NsiError => GenericFailedType)
  extends FiniteStateMachine[ReservationState, ReservationStateMachineData, InboundMessage, OutboundMessage](
    InitialReservationState,
    ReservationStateMachineData(
      initialReserve,
      Option(initialReserve.body.body.getGlobalReservationId()).map(URI.create(_)),
      Option(initialReserve.body.body.getDescription()),
      initialReserve.body.criteria)) {

  when(InitialReservationState) {
    case Event(FromRequester(NsiProviderMessage(_, message: InitialReserve)), _) =>
      goto(PathComputationState)
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data) =>
      goto(CheckingReservationState) using data.receivedSegments(children.segments)
    case Event(FromPce(message: PathComputationFailed), _) =>
      goto(FailedReservationState)
    case Event(AckFromPce(failure: PceFailed), data) =>
      goto(FailedReservationState) using data.copy(pceError = Some(NsiError.TopologyError.copy(text = s"PCE failed to accept request ${failure.status} (${failure.statusText})")))
    case Event(AckFromPce(_: PceAccepted), _) =>
      stay
  }

  when(CheckingReservationState) {
    case Event(AckFromProvider(NsiProviderMessage(headers, ReserveResponse(connectionId))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      stay using data.updateChildStatus(headers.correlationId, CheckingReservationState)
    case Event(AckFromProvider(NsiProviderMessage(headers, ServiceException(serviceException))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .receivedReserveReply(headers.correlationId)
        .updateChildStatus(headers.correlationId, FailedReservationState, Some(serviceException))
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveConfirmed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .receivedReserveReply(headers.correlationId)
        .updateChild(headers.correlationId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .receivedReserveReply(headers.correlationId)
        .updateChild(headers.correlationId, FailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(HeldReservationState) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveCommit)), data) =>
      val newData = data.startProcessingNewCommand(message, CommittingReservationState, children)
      goto(CommittingReservationState) using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState, children)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveTimeout)), data) =>
      val newData = data.updateChild(children.initialCorrelationIdFor(message.connectionId), reservationState = TimeoutReservationState, childTimeout = Some(message.timeout))
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommittingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitConfirmed)), data) if data.childHasState(message.connectionId, CommittingReservationState, children) =>
      val newData = data.updateChild(children.initialCorrelationIdFor(message.connectionId), ReservedReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitFailed)), data) if data.childHasState(message.connectionId, CommittingReservationState, children) =>
      val newData = data.updateChild(children.initialCorrelationIdFor(message.connectionId), CommitFailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(AbortingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveAbortConfirmed)), data) if data.childHasState(message.connectionId, AbortingReservationState, children) =>
      val newData = data.updateChild(children.initialCorrelationIdFor(message.connectionId), AbortedReservationState)
      goto(newData.aggregatedReservationState) using newData
  }

  when(TimeoutReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveTimeout)), data) =>
      val newData = data.updateChild(children.initialCorrelationIdFor(message.connectionId), reservationState = TimeoutReservationState, childTimeout = Some(message.timeout))
      stay using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState, children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(FailedReservationState) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState, children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(ReservedReservationState)(PartialFunction.empty)
  when(CommitFailedReservationState)(PartialFunction.empty)
  when(AbortedReservationState)(PartialFunction.empty)

  whenUnhandled {
    case Event(AckFromProvider(_), _) => stay
  }

  onTransition {
    case InitialReservationState -> PathComputationState =>
      Seq(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, initialReserve.body.criteria.getSchedule(), ServiceType(initialReserve.body.criteria.getServiceType(), initialReserve.body.service), pathComputationAlgorithm, initialReserve.headers.connectionTrace)))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData
      children.segments.map {
        case (correlationId, segment) =>
          val service = segment.serviceType.service
          val criteria = new ReservationRequestCriteriaType().
            withPointToPointService(service).
            withSchedule(data.criteria.getSchedule()).
            withServiceType(data.criteria.getServiceType()).
            withVersion(data.criteria.getVersion())
          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.map(_.toASCIIString()).orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria)

          val headers = newInitialReserveNsiHeaders(segment.provider).copy(correlationId = correlationId)

          ToProvider(NsiProviderMessage(headers, InitialReserve(reserveType, Conversion.invert(criteria).get, service)), segment.provider)
      }
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.pceError.getOrElse(NsiError.NoPathFound))))
    case CheckingReservationState -> FailedReservationState =>
      respond(ReserveFailed(failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava))))
    case CheckingReservationState -> HeldReservationState =>
      respond(ReserveConfirmed(id, nextStateData.criteria))
    case HeldReservationState -> CommittingReservationState =>
      children.childConnections.collect {
        case (seg, _, Some(connectionId)) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveCommit(connectionId)), seg.provider)
      }.toVector
    case (HeldReservationState | FailedReservationState | TimeoutReservationState) -> AbortingReservationState =>
      children.childConnections.collect {
        case (seg, _, Some(connectionId)) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveAbort(connectionId)), seg.provider)
      }.toVector
    case HeldReservationState -> TimeoutReservationState =>
      val (_, timeout) = nextStateData.childTimeouts.head
      respond(ReserveTimeout(new ReserveTimeoutRequestType()
        .withConnectionId(id)
        .withNotificationId(newNotificationId())
        .withTimeStamp(timeout.getTimeStamp())
        .withTimeoutValue(timeout.getTimeoutValue())
        .withOriginatingConnectionId(timeout.getOriginatingConnectionId())
        .withOriginatingNSA(timeout.getOriginatingNSA())))
    case CommittingReservationState -> ReservedReservationState =>
      respond(ReserveCommitConfirmed(id))
    case CommittingReservationState -> CommitFailedReservationState =>
      respond(ReserveCommitFailed(failed(NsiError.InternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava))))
    case ReservedReservationState -> CheckingReservationState =>
      throw new IllegalStateException("modify reserve not implemented yet")
    case (HeldReservationState | FailedReservationState | TimeoutReservationState | AbortingReservationState) -> AbortedReservationState =>
      respond(ReserveAbortConfirmed(id))
  }

  def childConnectionStateByInitialCorrelationId(correlationId: CorrelationId): ReservationStateEnumType = {
    stateData.childConnectionStates.getOrElse(correlationId, CheckingReservationState).jaxb
  }

  def reservationState = stateName.jaxb
  def criteria = stateData.criteria
  def version = stateData.criteria.getVersion()

  private def respond(body: NsiRequesterOperation) = Seq(ToRequester(nextStateData.currentCommand reply body))
}
