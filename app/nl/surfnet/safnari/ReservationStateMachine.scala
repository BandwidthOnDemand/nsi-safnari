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
  globalReservationId: Option[String],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  segments: Vector[(CorrelationId, ComputedSegment)] = Vector.empty,
  connections: Map[CorrelationId, ConnectionId] = Map.empty,
  childConnectionStates: Map[ConnectionId, ReservationState] = Map.empty,
  childExceptions: Map[ConnectionId, ServiceExceptionType] = Map.empty,
  childTimeouts: Map[ConnectionId, ReserveTimeoutRequestType] = Map.empty) {

  def awaitingConnectionId = segments.map(_._1).toSet -- connections.keySet

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
    copy(connections = connections.updated(correlationId, connectionId))
  }

  def updateChild(connectionId: ConnectionId, reservationState: ReservationState, childException: Option[ServiceExceptionType] = None, childTimeout: Option[ReserveTimeoutRequestType] = None) =
    copy(
      childConnectionStates = childConnectionStates.updated(connectionId, reservationState),
      childExceptions = childException.map(childExceptions.updated(connectionId, _)).getOrElse(childExceptions - connectionId),
      childTimeouts = childTimeout.map(childTimeouts.updated(connectionId, _)).getOrElse(childTimeouts))

  def startProcessingNewCommand(command: NsiProviderMessage[NsiProviderOperation], transitionalState: ReservationState) =
    copy(
      currentCommand = command,
      childConnectionStates = childConnectionStates.map { _._1 -> transitionalState },
      childExceptions = Map.empty)

  def childHasState(connectionId: ConnectionId, state: ReservationState) =
    childConnectionStates.getOrElse(connectionId, CheckingReservationState) == state

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
  failed: NsiError => GenericFailedType)
  extends FiniteStateMachine[ReservationState, ReservationStateMachineData, InboundMessage, OutboundMessage](
    InitialReservationState,
    ReservationStateMachineData(
      initialReserve,
      Option(initialReserve.body.body.getGlobalReservationId()),
      Option(initialReserve.body.body.getDescription()),
      initialReserve.body.criteria)) {

  when(InitialReservationState) {
    case Event(FromRequester(NsiProviderMessage(_, message: InitialReserve)), _) =>
      goto(PathComputationState)
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data) =>
      val segments = message.segments.map(newCorrelationId() -> _)
      goto(CheckingReservationState) using data.copy(segments = segments.toVector)
    case Event(FromPce(message: PathComputationFailed), _) =>
      goto(FailedReservationState)
  }

  when(CheckingReservationState) {
    case Event(AckFromProvider(NsiProviderMessage(headers, ReserveResponse(connectionId))), data) if data.childHasState(connectionId, CheckingReservationState) =>
      stay using data
        .receivedConnectionId(headers.correlationId, connectionId)
        .updateChild(connectionId, CheckingReservationState)
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveConfirmed)), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
        .receivedConnectionId(headers.correlationId, message.connectionId)
        .updateChild(message.connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(message.connectionId, CheckingReservationState) =>
      val newData = data
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
    case Event(AckFromProvider(NsiProviderMessage(_, _: ReserveResponse)), _) => stay
    case Event(AckFromProvider(NsiProviderMessage(_, _: GenericAck)), _) => stay
    case Event(AckFromProvider(NsiProviderMessage(headers, ServiceException(exception))), data) =>
      ???
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
            withGlobalReservationId(data.globalReservationId.orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria)

          ToProvider(NsiProviderMessage(newNsiHeaders(segment.provider).copy(correlationId = correlationId), InitialReserve(reserveType, Conversion.invert(criteria).right.get, service)), segment.provider)
      }
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(failed(NsiError.PathComputationNoPath)))
    case CheckingReservationState -> FailedReservationState =>
      respond(ReserveFailed(failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava))))
    case CheckingReservationState -> HeldReservationState =>
      respond(ReserveConfirmed(id, nextStateData.criteria))
    case HeldReservationState -> CommittingReservationState =>
      nextStateData.connections.map {
        case (correlationId, connectionId) =>
          val seg = nextStateData.segmentByCorrelationId(correlationId)
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveCommit(connectionId)), seg.provider)
      }.toVector
    case HeldReservationState -> AbortingReservationState =>
      nextStateData.connections.map {
        case (correlationId, connectionId) =>
          val seg = nextStateData.segmentByCorrelationId(correlationId)
          ToProvider(NsiProviderMessage(newNsiHeaders(seg.provider), ReserveAbort(connectionId)), seg.provider)
      }.toVector
    case HeldReservationState -> TimeoutReservationState =>
      val (timedOutConnectionId, timeout) = nextStateData.childTimeouts.head
      respond(ReserveTimeout(timeout.withConnectionId(id)))
//          new ReserveTimeoutRequestType()
//        .withConnectionId(id)
//        .withTimeStamp(timeout.getTimeStamp())
//        .withTimeoutValue(timeout.getTimeoutValue())
//        .withOriginatingConnectionId(timedOutConnectionId)
//        .withOriginatingNSA(timeout.getOriginatingNSA()))
    case CommittingReservationState -> ReservedReservationState =>
      respond(ReserveCommitConfirmed(id))
    case CommittingReservationState -> CommitFailedReservationState =>
      respond(ReserveCommitFailed(failed(NsiError.InternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava))))
    case ReservedReservationState -> CheckingReservationState => ???
    case FailedReservationState -> AbortingReservationState   => ???
    case AbortingReservationState -> ReservedReservationState => ???
  }

  def segmentKnown(connectionId: ConnectionId) = stateData.childConnectionStates.contains(connectionId)
  def childConnectionState(connectionId: ConnectionId): ReservationStateEnumType = stateData.childConnectionStates(connectionId).jaxb
  def childConnections: Seq[(ComputedSegment, Option[ConnectionId])] = stateData.segments.map {
    case (correlationId, segment) =>
      (segment, stateData.connections.get(correlationId))
  }
  def reservationState = stateName.jaxb
  def criteria = stateData.criteria
  def version = stateData.criteria.getVersion()

  private def respond(body: NsiRequesterOperation) = Seq(ToRequester(nextStateData.currentCommand reply body))
}
