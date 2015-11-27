/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
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

final case class ConnectionCriteria(pending: Option[Either[ReservationRequestCriteriaType, ReservationConfirmCriteriaType]], committed: Option[ReservationConfirmCriteriaType]) {
  def withRequested(requested: ReservationRequestCriteriaType) = copy(pending = Some(Left(requested)))
  def withHeld(held: ReservationConfirmCriteriaType) = copy(pending = Some(Right(held)))
  def commit = copy(pending = None, committed = pending.flatMap(_.right.toOption))
  def abort = copy(pending = None)

  def requested = pending.flatMap(_.left.toOption)
  def confirmed = pending.flatMap(_.right.toOption)

  def pendingVersion: Int = {
    pending.flatMap(_.fold(
        requested => if (requested.getVersion eq null) None else Some(requested.getVersion.intValue),
        confirmed => Some(confirmed.getVersion)))
      .orElse(committed.map(_.getVersion + 1))
      .getOrElse(1)
  }
}
object ConnectionCriteria {
  val Initial = ConnectionCriteria(None, None)
}

case class ReservationStateMachineData(
    currentCommand: NsiProviderMessage[NsiProviderOperation],
    globalReservationId: Option[GlobalReservationId],
    description: Option[String],
    criteria: ConnectionCriteria,
    segments: Seq[(CorrelationId, ComputedSegment)] = Seq.empty,
    childConnectionStates: Map[CorrelationId, ReservationState] = Map.empty,
    childExceptions: Map[CorrelationId, ServiceExceptionType] = Map.empty,
    childConnectionCriteria: Map[CorrelationId, ConnectionCriteria] = Map.empty,
    pceError: Option[NsiError] = None,
    reserveError: Option[NsiError] = None) {

  def receivedSegments(segments: Seq[(CorrelationId, ComputedSegment)]) = {
    val Some(Left(requestedCriteria)) = criteria.pending
    val childCriteria: Map[CorrelationId, ConnectionCriteria] = segments.map {
      case (correlationId, segment) =>
        val service = segment.serviceType.service
        val criteria = new ReservationRequestCriteriaType().
          withPointToPointService(service).
          withSchedule(requestedCriteria.getSchedule()).
          withServiceType(requestedCriteria.getServiceType()).
          withVersion(pendingVersion)
        requestedCriteria.getPointToPointService().foreach(ptp =>
          criteria.getPointToPointService().get.withParameter(ptp.getParameter))
        correlationId -> ConnectionCriteria.Initial.withRequested(criteria)
    }(collection.breakOut)

    copy(
      segments = segments,
      childConnectionCriteria = childCriteria,
      childConnectionStates = segments.map { case (correlationId, _) => correlationId -> CheckingReservationState }(collection.breakOut))
  }

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
    else if (childConnectionStates.values.forall(_ == ReservedReservationState)) ReservedReservationState
    else if (childConnectionStates.values.forall(_ == AbortedReservationState)) AbortedReservationState
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

  def updateChild(initialCorrelationId: CorrelationId, reservationState: ReservationState, exception: Option[ServiceExceptionType] = None): ReservationStateMachineData = {
    copy(
      childConnectionStates = childConnectionStates.updated(initialCorrelationId, reservationState),
      childExceptions = exception.fold(childExceptions - initialCorrelationId)(exception => childExceptions.updated(initialCorrelationId, exception)))
  }

  def updateChildByConnectionId(children: ChildConnectionIds, connectionId: ConnectionId, reservationState: ReservationState, exception: Option[ServiceExceptionType] = None): ReservationStateMachineData = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"missing child connection id for $connectionId"))
    updateChild(correlationId, reservationState, exception)
  }

  def modifyChildCriteria(children: ChildConnectionIds, connectionId: ConnectionId)(f: ConnectionCriteria => ConnectionCriteria): ReservationStateMachineData = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"missing child connection id for $connectionId"))
    modifyChildCriteria(correlationId)(f)
  }

  def modifyChildCriteria(initialCorrelationId: CorrelationId)(f: ConnectionCriteria => ConnectionCriteria): ReservationStateMachineData = {
    copy(childConnectionCriteria = childConnectionCriteria.updated(initialCorrelationId, f(childConnectionCriteria.getOrElse(initialCorrelationId, ConnectionCriteria.Initial))))
  }

  private def requestedToConfirmCriteria: Option[ReservationConfirmCriteriaType] = criteria.requested.flatMap { requestedCriteria =>
    (criteria.committed map requestedCriteria.toModifiedConfirmCriteria getOrElse requestedCriteria.toInitialConfirmCriteria(
      childConnectionCriteria(segments.head._1).confirmed.get.getPointToPointService.get.getSourceSTP,
      childConnectionCriteria(segments.last._1).confirmed.get.getPointToPointService.get.getDestSTP)).toOption
  }

  def requestedCriteriaToHeld = copy(criteria = requestedToConfirmCriteria.fold(criteria.abort)(criteria.withHeld))

  def commitPendingCriteria = copy(criteria = criteria.commit)

  def pendingVersion = criteria.pendingVersion
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
        ConnectionCriteria.Initial)) {

  when(InitialReservationState) {
    case Event(FromRequester(NsiProviderMessage(_, message: InitialReserve)), data) =>
      goto(PathComputationState) using data.copy(criteria = data.criteria.withRequested(message.criteria))
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
        .modifyChildCriteria(children, message.connectionId)(_.withHeld(message.criteria))
      val newData2 = if (newData.aggregatedReservationState == HeldReservationState) newData.requestedCriteriaToHeld else newData
      goto(newData2.aggregatedReservationState) using newData2
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, FailedReservationState, Some(message.failed.getServiceException()))
        .modifyChildCriteria(children, message.connectionId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
    case Event(AckFromProvider(NsiProviderMessage(headers, ServiceException(serviceException))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, FailedReservationState, Some(serviceException))
        .modifyChildCriteria(headers.correlationId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(HeldReservationState) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveCommit)), data) =>
      val newData = data.startProcessingNewCommand(message, CommittingReservationState, children)
      goto(CommittingReservationState) using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState, children).copy(criteria = data.criteria.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommittingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitConfirmed)), data) if data.childHasState(children, message.connectionId, CommittingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, message.connectionId, ReservedReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.commit)
      val newData2 = if (newData.aggregatedReservationState == ReservedReservationState) newData.commitPendingCriteria else newData
      goto(newData2.aggregatedReservationState) using newData2
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveCommitFailed)), data) if data.childHasState(children, message.connectionId, CommittingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, message.connectionId, CommitFailedReservationState, Some(message.failed.getServiceException()))
        .modifyChildCriteria(children, message.connectionId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(AbortingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReserveAbortConfirmed)), data) if data.childHasState(children, message.connectionId, AbortingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, message.connectionId, if (data.criteria.committed.isDefined) ReservedReservationState else AbortedReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(FailedReservationState) {
    case Event(FromRequester(command @ NsiProviderMessage(_, ReserveAbort(_))), data) =>
      val newData =
        if (data.reserveError.isDefined)
          data.copy(currentCommand = command, reserveError = None, criteria = data.criteria.abort)
        else
          data.startProcessingNewCommand(command, AbortingReservationState, children).copy(criteria = data.criteria.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(ReservedReservationState) {
    case Event(FromRequester(command @ NsiProviderMessage(headers, ModifyReserve(reserve))), data) =>
      val pendingCriteria = reserve.getCriteria

      val error = validateModify(pendingCriteria, data.criteria.committed.get)

      val newData = error.map { error =>
        data.copy(currentCommand = command, childExceptions = Map.empty, reserveError = Some(error))
      }.getOrElse {
        val data2 = data
          .startProcessingNewCommand(command, ModifyingReservationState, children)
          .copy(criteria = data.criteria.withRequested(pendingCriteria))

        data2.copy(
          childConnectionCriteria = data.segments.map {
            case (initialCorrelationId, segment) =>
              val childCriteria = data.childConnectionCriteria(initialCorrelationId)
              val service = segment.serviceType.service.shallowCopy
              pendingCriteria.getPointToPointService().foreach { ptp =>
                service.setCapacity(ptp.getCapacity)
              }

              val criteria = new ReservationRequestCriteriaType()
                .withPointToPointService(service)
                .withSchedule(pendingCriteria.getSchedule())
                .withServiceType(segment.serviceType.serviceType)
                .withVersion(data2.pendingVersion)

              initialCorrelationId -> childCriteria.withRequested(criteria)
          }(collection.breakOut))
      }

      goto(newData.aggregatedReservationState) using newData
  }

  when(ModifyingReservationState) {
    case Event(FromProvider(NsiRequesterMessage(headers, message @ ReserveConfirmed(connectionId, _))), data) if data.childHasState(children, connectionId, ModifyingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, connectionId, HeldReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.withHeld(message.criteria))
      val newData2 = if (newData.aggregatedReservationState == HeldReservationState) newData.requestedCriteriaToHeld else newData
      goto(newData2.aggregatedReservationState) using newData2
  }

  when(CommitFailedReservationState)(PartialFunction.empty)
  when(AbortedReservationState)(PartialFunction.empty)

  whenUnhandled {
    case Event(AckFromProvider(_), _) => stay
  }

  onTransition {
    case InitialReservationState -> PathComputationState =>
      val criteria = nextStateData.criteria.requested.get
      Seq(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, criteria.getSchedule(), ServiceType(criteria.getServiceType(), criteria.getPointToPointService().get), pathComputationAlgorithm, initialReserve.headers.connectionTrace)))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData
      children.segments.map {
        case (correlationId, segment) =>
          val criteria = nextStateData.childConnectionCriteria(correlationId)

          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.map(_.toASCIIString()).orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria.requested.get)

          val headers = newInitialReserveNsiHeaders(segment.provider).copy(correlationId = correlationId)

          ToProvider(NsiProviderMessage(headers, InitialReserve(reserveType)), segment.provider)
      }
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.pceError.getOrElse(NsiError.NoPathFound))))
    case CheckingReservationState -> FailedReservationState =>
      respond(ReserveFailed(failed(NsiError.ChildError).tap(_.getServiceException().withChildException(nextStateData.childExceptions.values.toSeq.asJava))))
    case (CheckingReservationState | ModifyingReservationState) -> HeldReservationState =>
      respond(ReserveConfirmed(id, nextStateData.criteria.confirmed.get))
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
        case (segment, initialCorrelationId, Some(childConnectionId)) =>
          val criteria = nextStateData.childConnectionCriteria(initialCorrelationId)

          val reserveType = new ReserveType()
            .withConnectionId(childConnectionId)
            .withGlobalReservationId(data.globalReservationId.map(_.toASCIIString()).orNull)
            .withDescription(data.description.orNull)
            .withCriteria(criteria.requested.get)

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

  def childConnectionCriteria(correlationId: CorrelationId): ConnectionCriteria = {
    nextStateData.childConnectionCriteria.getOrElse(correlationId, ConnectionCriteria.Initial)
  }

  def reservationState = nextStateName.jaxb
  def pendingCriteria = nextStateData.criteria.requested
  def committedCriteria = nextStateData.criteria.committed
  def committedVersion = committedCriteria.map(_.getVersion()).getOrElse(0)

  private def respond(body: NsiRequesterOperation) = Seq(ToRequester(nextStateData.currentCommand reply body))

  private def validateModify(requestedCriteria: ReservationRequestCriteriaType, committedCriteria: ReservationConfirmCriteriaType) = {
    val committedP2Ps = committedCriteria.getPointToPointService.get
    val requestedP2Ps = requestedCriteria.getPointToPointService.get
    if ((requestedCriteria.getVersion ne null) && committedVersion >= requestedCriteria.getVersion)
      Some(NsiError.PayloadError.copy(text = s"requested version ${requestedCriteria.getVersion} must be greater than committed version $committedVersion"))
    else if (!(committedP2Ps.sourceStp isCompatibleWith requestedP2Ps.sourceStp))
      Some(NsiError.PayloadError.copy(text = s"committed source STP ${committedP2Ps.sourceStp} is not compatible with requested source STP ${requestedP2Ps.sourceStp}"))
    else if (!(committedP2Ps.destStp isCompatibleWith requestedP2Ps.destStp))
      Some(NsiError.PayloadError.copy(text = s"committed destination STP ${committedP2Ps.destStp} is not compatible with requested destination STP ${requestedP2Ps.destStp}"))
    else
      None
  }
}
