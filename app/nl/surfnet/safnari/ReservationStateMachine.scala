package nl.surfnet.safnari

import java.net.URI

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

import scala.collection.JavaConverters._

sealed abstract class ReservationState(val jaxb: ReservationStateEnumType)
case object InitialReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object FailedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_FAILED)
case object ReservedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object PathComputationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object CheckingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object ModifyingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_CHECKING)
case object HeldReservationState extends ReservationState(ReservationStateEnumType.RESERVE_HELD)
case object CommittingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_COMMITTING)
case object CommitFailedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
case object AbortingReservationState extends ReservationState(ReservationStateEnumType.RESERVE_ABORTING)
case object AbortedReservationState extends ReservationState(ReservationStateEnumType.RESERVE_START)
// RESERVE_TIMEOUT is an ultimate provider state only so not applicable to the aggregator.
//case object TimeoutReservationState extends ReservationState(ReservationStateEnumType.RESERVE_TIMEOUT)

case class ReservationStateMachineData(
  currentCommand: NsiProviderMessage[NsiProviderOperation],
  globalReservationId: Option[GlobalReservationId],
  description: Option[String],
  pendingCriteria: Option[ReservationRequestCriteriaType],
  committedCriteria: Option[ReservationConfirmCriteriaType] = None,
  segments: Seq[(CorrelationId, ComputedSegment)] = Seq.empty,
  childConnectionStates: Map[CorrelationId, ReservationState] = Map.empty,
  childExceptions: Map[CorrelationId, ServiceExceptionType] = Map.empty,
  pceError: Option[NsiError] = None,
  reserveError: Option[NsiError] = None) {

  def receivedSegments(segments: Seq[(CorrelationId, ComputedSegment)]) =
    copy(
      segments = segments,
      childConnectionStates = segments.map { case (correlationId, _) => correlationId -> CheckingReservationState }(collection.breakOut))

  def aggregatedReservationState: ReservationState =
    if (segments.isEmpty) PathComputationState
    else if (reserveError.isDefined) FailedReservationState
    else if (childConnectionStates.values.exists(_ == CheckingReservationState)) CheckingReservationState
    else if (childConnectionStates.values.exists(_ == ModifyingReservationState)) ModifyingReservationState
    else if (childConnectionStates.values.exists(_ == FailedReservationState)) FailedReservationState
    else if (childConnectionStates.values.exists(_ == CommittingReservationState)) CommittingReservationState
    else if (childConnectionStates.values.exists(_ == CommitFailedReservationState)) CommitFailedReservationState
    else if (childConnectionStates.values.exists(_ == AbortingReservationState)) AbortingReservationState
    else if (childConnectionStates.values.forall(_ == HeldReservationState)) HeldReservationState
    else if (childConnectionStates.values.forall(_ == AbortedReservationState)) AbortedReservationState
    else if (childConnectionStates.values.forall(_ == ReservedReservationState)) ReservedReservationState
    else throw new IllegalStateException(s"cannot determine aggregated state from child states ${childConnectionStates.values.mkString(",")}")

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

  def childHasState(initialCorrelationId: CorrelationId, state: ReservationState): Boolean = {
    childConnectionStates.get(initialCorrelationId).exists(_ == state)
  }

  def childHasState(children: ChildConnectionIds, connectionId: ConnectionId, state: ReservationState): Boolean = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"missing child connection id for $connectionId"))
    childHasState(correlationId, state)
  }

  def updateChild(initialCorrelationId: CorrelationId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None): ReservationStateMachineData = {
    copy(
      childConnectionStates = childConnectionStates.updated(initialCorrelationId, reservationState),
      childExceptions = childException.fold(childExceptions - initialCorrelationId)(exception => childExceptions.updated(initialCorrelationId, exception)))
  }

  def updateChild(children: ChildConnectionIds, connectionId: ConnectionId, reservationState: ReservationState): ReservationStateMachineData = {
    updateChild(children, connectionId, reservationState, None)
  }

  def updateChild(children: ChildConnectionIds, connectionId: ConnectionId, reservationState: ReservationState, childException: Option[ServiceExceptionType]): ReservationStateMachineData = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"missing child connection id for $connectionId"))
    updateChild(correlationId, reservationState, childException)
  }

  def pendingToConfirmCriteria = pendingCriteria.toTry(ErrorMessage("no pending criteria")).flatMap { pendingCriteria =>
    pendingCriteria.toConfirmCriteria(
      pendingCriteria.getPointToPointService().get.getSourceSTP,
      pendingCriteria.getPointToPointService().get.getDestSTP,
      if (pendingCriteria.getVersion eq null) committedCriteria.map(_.getVersion).getOrElse(0) else pendingCriteria.getVersion + 1)
  }

  def commitPendingCriteria = copy(pendingCriteria = None, committedCriteria = pendingToConfirmCriteria.toOption)
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
      Some(initialReserve.body.criteria))) {

  when(InitialReservationState) {
    case Event(FromRequester(NsiProviderMessage(_, message: InitialReserve)), data) =>
      goto(PathComputationState) using data.copy(pendingCriteria = Some(message.criteria))
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data) =>
      goto(CheckingReservationState) using data.receivedSegments(children.segments)
    case Event(FromPce(message: PathComputationFailed), data) =>
      goto(FailedReservationState) using data.copy(pceError = Some(message.error))
    case Event(AckFromPce(failure: PceFailed), data) =>
      goto(FailedReservationState) using data.copy(pceError = Some(NsiError.TopologyError.copy(text = s"PCE failed to accept request ${failure.status} (${failure.statusText})")))
    case Event(AckFromPce(_: PceAccepted), _) =>
      stay
  }

  when(CheckingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveConfirmed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, FailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
    case Event(AckFromProvider(NsiProviderMessage(headers, ServiceException(serviceException))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, FailedReservationState, Some(serviceException))
      goto(newData.aggregatedReservationState) using newData
  }

  when(HeldReservationState) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveCommit)), data) =>
      val newData = data.startProcessingNewCommand(message, CommittingReservationState, children)
      goto(CommittingReservationState) using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState, children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommittingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitConfirmed)), data) if data.childHasState(children, message.connectionId, CommittingReservationState) =>
      val newData1 = data.updateChild(children, message.connectionId, ReservedReservationState)
      val newData2 = if (newData1.aggregatedReservationState == ReservedReservationState) newData1.commitPendingCriteria else newData1
      goto(newData2.aggregatedReservationState) using newData2
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitFailed)), data) if data.childHasState(children, message.connectionId, CommittingReservationState) =>
      val newData = data.updateChild(children, message.connectionId, CommitFailedReservationState, Some(message.failed.getServiceException()))
      goto(newData.aggregatedReservationState) using newData
  }

  when(AbortingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveAbortConfirmed)), data) if data.childHasState(children, message.connectionId, AbortingReservationState) =>
      val newData = data.updateChild(children, message.connectionId, AbortedReservationState)
      goto(newData.aggregatedReservationState) using newData
  }

  when(FailedReservationState) {
    case Event(FromRequester(command @ NsiProviderMessage(_, ReserveAbort(_))), data) =>
      val newData =
        if (data.reserveError.isDefined)
          data.copy(currentCommand = command, reserveError = None, pendingCriteria = None)
        else
          data.startProcessingNewCommand(command, AbortingReservationState, children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(ReservedReservationState) {
    case Event(FromRequester(command @ NsiProviderMessage(headers, ModifyReserve(reserve))), data) =>
      val newData =
        if ((reserve.getCriteria.getVersion ne null) && version >= reserve.getCriteria.getVersion)
          data.copy(currentCommand = command, childExceptions = Map.empty, reserveError = Some(NsiError.PayloadError), pendingCriteria = Some(reserve.getCriteria))
        else
          data.copy(pendingCriteria = Some(reserve.getCriteria)).startProcessingNewCommand(command, ModifyingReservationState, children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(ModifyingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message @ ReserveConfirmed(connectionId, _))), data) if data.childHasState(children, connectionId, ModifyingReservationState) =>
      val newData = data
        .updateChild(children, connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommitFailedReservationState)(PartialFunction.empty)
  when(AbortedReservationState)(PartialFunction.empty)

  whenUnhandled {
    case Event(AckFromProvider(_), _) => stay
  }

  onTransition {
    case InitialReservationState -> PathComputationState =>
      Seq(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, nextStateData.pendingCriteria.get.getSchedule(), ServiceType(nextStateData.pendingCriteria.get.getServiceType(), nextStateData.pendingCriteria.get.getPointToPointService().get), pathComputationAlgorithm, initialReserve.headers.connectionTrace)))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData
      val pendingCriteria = data.pendingCriteria.get
      children.segments.map {
        case (correlationId, segment) =>
          val service = segment.serviceType.service
          val criteria = new ReservationRequestCriteriaType().
            withPointToPointService(service).
            withSchedule(pendingCriteria.getSchedule()).
            withServiceType(pendingCriteria.getServiceType()).
            withVersion(pendingCriteria.getVersion())
          pendingCriteria.getPointToPointService().foreach(ptp =>
            criteria.getPointToPointService().get.withParameter(ptp.getParameter))

          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.map(_.toASCIIString()).orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria)

          val headers = newInitialReserveNsiHeaders(segment.provider).copy(correlationId = correlationId)

          ToProvider(NsiProviderMessage(headers, InitialReserve(reserveType)), segment.provider)
      }
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.pceError.getOrElse(NsiError.NoPathFound))))
    case CheckingReservationState -> FailedReservationState =>
      respond(ReserveFailed(failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava))))
    case (CheckingReservationState | ModifyingReservationState) -> HeldReservationState =>
      respond(ReserveConfirmed(id, nextStateData.pendingToConfirmCriteria.get))
    case HeldReservationState -> CommittingReservationState =>
      children.childConnections.collect {
        case (seg, _, Some(connectionId)) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveCommit(connectionId)), seg.provider)
      }.toVector
    case (HeldReservationState | FailedReservationState) -> AbortingReservationState =>
      children.childConnections.collect {
        case (seg, _, Some(connectionId)) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveAbort(connectionId)), seg.provider)
      }.toVector
    case CommittingReservationState -> ReservedReservationState =>
      respond(ReserveCommitConfirmed(id))
    case CommittingReservationState -> CommitFailedReservationState =>
      respond(ReserveCommitFailed(failed(NsiError.InternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava))))
    case ReservedReservationState -> ModifyingReservationState =>
      val data = nextStateData
      children.childConnections.collect {
        case (segment, _, Some(childConnectionId)) =>
          val service = segment.serviceType.service.shallowCopy
          val pendingCriteria = data.pendingCriteria.get
          pendingCriteria.getPointToPointService().foreach { ptp =>
            service.setCapacity(ptp.getCapacity)
            service.withParameter(ptp.getParameter)
          }

          val criteria = new ReservationRequestCriteriaType()
            .withPointToPointService(service)
            .withSchedule(pendingCriteria.getSchedule())
            .withServiceType(pendingCriteria.getServiceType())
            .withVersion(pendingCriteria.getVersion())

          val reserveType = new ReserveType()
            .withConnectionId(childConnectionId)
            .withGlobalReservationId(data.globalReservationId.map(_.toASCIIString()).orNull)
            .withDescription(data.description.orNull)
            .withCriteria(criteria)

          val headers = newNsiHeaders(segment.provider)

          ToProvider(NsiProviderMessage(headers, ModifyReserve(reserveType)), segment.provider)
      }
    case (InitialReservationState | ReservedReservationState) -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.reserveError.get)))
    case (HeldReservationState | FailedReservationState | AbortingReservationState) -> (AbortedReservationState | ReservedReservationState) =>
      respond(ReserveAbortConfirmed(id))
  }

  def childConnectionStateByInitialCorrelationId(correlationId: CorrelationId): ReservationStateEnumType = {
    stateData.childConnectionStates.getOrElse(correlationId, CheckingReservationState).jaxb
  }

  def reservationState = nextStateName.jaxb
  def pendingCriteria = nextStateData.pendingCriteria
  def committedCriteria = nextStateData.committedCriteria
  def version = nextStateData.committedCriteria.map(_.getVersion()).getOrElse(0)

  private def respond(body: NsiRequesterOperation) = Seq(ToRequester(nextStateData.currentCommand reply body))
}
