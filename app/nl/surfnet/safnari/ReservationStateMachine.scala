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
  currentCommand: NsiProviderMessage[NsiProviderOperation],
  globalReservationId: Option[GlobalReservationId],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  segments: Vector[(CorrelationId, ComputedSegment)] = Vector.empty,
  connectionByInitialCorrelationId: Map[CorrelationId, ConnectionId] = Map.empty,
  awaitingReserveReply: Set[CorrelationId] = Set.empty,
  initialCorrelationIdByConnectionId: Map[ConnectionId, CorrelationId] = Map.empty,
  childConnectionStates: Map[CorrelationId, ReservationState] = Map.empty,
  childExceptions: Map[CorrelationId, ServiceExceptionType] = Map.empty,
  childTimeouts: Map[CorrelationId, ReserveTimeoutRequestType] = Map.empty,
  pceError: Option[NsiError] = None) {

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

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId) = {
    copy(
      connectionByInitialCorrelationId = connectionByInitialCorrelationId.updated(correlationId, connectionId),
      initialCorrelationIdByConnectionId = initialCorrelationIdByConnectionId.updated(connectionId, correlationId))
  }

  def updateChild(connectionId: ConnectionId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None, childTimeout: Option[ReserveTimeoutRequestType] = None): ReservationStateMachineData  = {
    val correlationId = initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"Unknown child connectionId $connectionId: $initialCorrelationIdByConnectionId"))
    updateChildStatus(correlationId, reservationState, childException, childTimeout)
  }
  def updateChildStatus(correlationId: CorrelationId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None, childTimeout: Option[ReserveTimeoutRequestType] = None): ReservationStateMachineData =
    copy(
      childConnectionStates = childConnectionStates.updated(correlationId, reservationState),
      childExceptions = childException.fold(childExceptions - correlationId)(exception => childExceptions.updated(correlationId, exception)),
      childTimeouts = childTimeout.fold(childTimeouts)(timeout => childTimeouts.updated(correlationId, timeout)))

  def startProcessingNewCommand(command: NsiProviderMessage[NsiProviderOperation], transitionalState: ReservationState) =
    copy(
      currentCommand = command,
      childConnectionStates = childConnectionStates.map { _._1 -> transitionalState },
      childExceptions = Map.empty)

  def childHasState(correlationId: CorrelationId, state: ReservationState): Boolean = {
    val currentState = childConnectionStates.get(correlationId)
    state == currentState.getOrElse(CheckingReservationState)
  }

  def childHasState(connectionId: ConnectionId, state: ReservationState): Boolean = {
    val correlationId = initialCorrelationIdByConnectionId.get(connectionId)
    val currentState = correlationId.flatMap(childConnectionStates.get)
    state == currentState.getOrElse(CheckingReservationState)
  }

  def segmentByCorrelationId(correlationId: CorrelationId): ComputedSegment =
    segments.find(_._1 == correlationId).map(_._2).getOrElse {
      throw new IllegalStateException(s"correlationId $correlationId doesn't map to a computed segment $segments")
    }
}

class ReservationStateMachine(
  id: ConnectionId,
  initialReserve: NsiProviderMessage[InitialReserve],
  pceReplyUri: URI,
  newCorrelationId: () => CorrelationId,
  newNsiHeaders: ProviderEndPoint => NsiHeaders,
  newNotificationId: () => Int,
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
      val segments = message.segments.map(newCorrelationId() -> _)
      goto(CheckingReservationState) using data.copy(segments = segments.toVector, awaitingReserveReply = segments.map(_._1).toSet)
    case Event(FromPce(message: PathComputationFailed), _) =>
      goto(FailedReservationState)
    case Event(AckFromPce(failure: PceFailed), data) =>
      goto(FailedReservationState) using data.copy(pceError = Some(NsiError.TopologyError.copy(text = s"PCE failed to accept request ${failure.status} (${failure.statusText})")))
    case Event(AckFromPce(_: PceAccepted), _) =>
      stay
  }

  when(CheckingReservationState) {
    case Event(AckFromProvider(NsiProviderMessage(headers, ReserveResponse(connectionId))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      stay using data
        .receivedConnectionId(headers.correlationId, connectionId)
        .updateChildStatus(headers.correlationId, CheckingReservationState)
    case Event(AckFromProvider(NsiProviderMessage(headers, ServiceException(serviceException))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val connectionId = Option(serviceException.getConnectionId())
      val newData = connectionId.fold(data)(data.receivedConnectionId(headers.correlationId, _))
        .receivedReserveReply(headers.correlationId)
        .updateChildStatus(headers.correlationId, FailedReservationState, Some(serviceException))
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveConfirmed)), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedReserveReply(headers.correlationId)
        .receivedConnectionId(headers.correlationId, message.connectionId)
        .updateChild(message.connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedReserveReply(headers.correlationId)
        .receivedConnectionId(headers.correlationId, message.connectionId)
        .updateChild(message.connectionId, FailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(HeldReservationState) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveCommit)), data) =>
      val newData = data.startProcessingNewCommand(message, CommittingReservationState)
      goto(CommittingReservationState) using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState)
      goto(AbortingReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveTimeout)), data) =>
      val newData = data.updateChild(connectionId = message.connectionId, reservationState = TimeoutReservationState, childTimeout = Some(message.timeout))
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommittingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitConfirmed)), data) if data.childHasState(message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(message.connectionId, ReservedReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitFailed)), data) if data.childHasState(message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(message.connectionId, CommitFailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(AbortingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveAbortConfirmed)), data) if data.childHasState(message.connectionId, AbortingReservationState) =>
      val newData = data.updateChild(message.connectionId, AbortedReservationState)
      goto(newData.aggregatedReservationState)
  }

  when(TimeoutReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveTimeout)), data) =>
      val newData = data.updateChild(connectionId = message.connectionId, reservationState = TimeoutReservationState, childTimeout = Some(message.timeout))
      stay using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState)
      goto(AbortingReservationState) using newData
  }

  when(ReservedReservationState)(PartialFunction.empty)
  when(FailedReservationState)(PartialFunction.empty)
  when(CommitFailedReservationState)(PartialFunction.empty)

  whenUnhandled {
    case Event(AckFromProvider(_), _) => stay
  }

  onTransition {
    case InitialReservationState -> PathComputationState =>
      Seq(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, initialReserve.body.criteria.getSchedule(), ServiceType(initialReserve.body.criteria.getServiceType(), initialReserve.body.service))))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData
      data.segments.map {
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

          ToProvider(NsiProviderMessage(newNsiHeaders(segment.provider).copy(correlationId = correlationId), InitialReserve(reserveType, Conversion.invert(criteria).get, service)), segment.provider)
      }
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.pceError.getOrElse(NsiError.NoPathFound))))
    case CheckingReservationState -> FailedReservationState =>
      respond(ReserveFailed(failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava))))
    case CheckingReservationState -> HeldReservationState =>
      respond(ReserveConfirmed(id, nextStateData.criteria))
    case HeldReservationState -> CommittingReservationState =>
      nextStateData.connectionByInitialCorrelationId.map {
        case (correlationId, connectionId) =>
          val seg = nextStateData.segmentByCorrelationId(correlationId)
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveCommit(connectionId)), seg.provider)
      }.toVector
    case HeldReservationState -> AbortingReservationState =>
      nextStateData.connectionByInitialCorrelationId.map {
        case (correlationId, connectionId) =>
          val seg = nextStateData.segmentByCorrelationId(correlationId)
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveAbort(connectionId)), seg.provider)
      }.toVector
    case HeldReservationState -> TimeoutReservationState =>
      val (_, timeout) = nextStateData.childTimeouts.head
      respond(ReserveTimeout(new ReserveTimeoutRequestType()
        .withConnectionId(id)
        .withNotificationId(newNotificationId())
        .withTimeStamp(timeout.getTimeStamp())
        .withTimeoutValue(timeout.getTimeoutValue())
        .withOriginatingConnectionId(timeout.getConnectionId())
        .withOriginatingNSA(timeout.getOriginatingNSA())))
    case CommittingReservationState -> ReservedReservationState =>
      respond(ReserveCommitConfirmed(id))
    case CommittingReservationState -> CommitFailedReservationState =>
      respond(ReserveCommitFailed(failed(NsiError.InternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava))))
    case ReservedReservationState -> CheckingReservationState => ???
    case FailedReservationState -> AbortingReservationState   => ???
    case AbortingReservationState -> ReservedReservationState => ???
  }

  def childConnectionState(connectionId: ConnectionId): ReservationStateEnumType = {
    val correlationId = stateData.initialCorrelationIdByConnectionId(connectionId)
    stateData.childConnectionStates(correlationId).jaxb
  }
  def childConnections: Seq[(ComputedSegment, Option[ConnectionId])] = stateData.segments.map {
    case (correlationId, segment) =>
      (segment, stateData.connectionByInitialCorrelationId.get(correlationId))
  }
  def reservationState = stateName.jaxb
  def criteria = stateData.criteria
  def version = stateData.criteria.getVersion()

  private def respond(body: NsiRequesterOperation) = Seq(ToRequester(nextStateData.currentCommand reply body))
}
