/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
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
import org.ogf.schemas.nsi._2015._04.connection.pathtrace.{ PathTraceType, PathType, SegmentType }

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
case object TimeoutReservationState extends ReservationState(ReservationStateEnumType.RESERVE_TIMEOUT)

case class ReservationStateMachineData(
    command: NsiProviderMessage[NsiProviderOperation],
    globalReservationId: Option[GlobalReservationId],
    description: Option[String],
    criteria: ConnectionCriteria,
    initialReserveAlgorithm: InitialReserveAlgorithm,
    segments: ComputedPathSegments = Seq.empty,
    childConnectionStates: Map[CorrelationId, ReservationState] = Map.empty,
    childExceptions: Map[CorrelationId, ServiceExceptionType] = Map.empty,
    childConnectionCriteria: Map[CorrelationId, ConnectionCriteria] = Map.empty,
    childReserveTimeouts: Vector[NsiRequesterMessage[ReserveTimeout]] = Vector.empty,
    childResponses: Map[CorrelationId, NsiRequesterMessage[NsiCommandReply]] = Map.empty,
    pceError: Option[NsiError] = None,
    reserveError: Option[NsiError] = None,
    aggregatedPathTrace: Option[PathTraceType] = None) {

  def receivedSegments(algorithm: PathComputationAlgorithm, segments: ComputedPathSegments) = copy(
    segments = segments,
    initialReserveAlgorithm = InitialReserveAlgorithm.forAlgorithm(algorithm).forSegments(segments)
  )

  def initialReserveConfirmed(correlationId: CorrelationId, criteria: ReservationConfirmCriteriaType) = {
    copy(initialReserveAlgorithm = initialReserveAlgorithm.reserveConfirmed(correlationId, criteria))
  }

  def clearNextSegments = copy(initialReserveAlgorithm = initialReserveAlgorithm.clearNextSegments)

  def reserveNextSegments: ReservationStateMachineData = {
    val Some(Left(requestedCriteria)) = criteria.pending

    val nextChildConnectionCriteria: Map[CorrelationId, ConnectionCriteria] = initialReserveAlgorithm.nextSegments.map {
      case (correlationId, segment) =>
        val criteria = new ReservationRequestCriteriaType()
          .withPointToPointService(segment.serviceType.service)
          .withSchedule(requestedCriteria.getSchedule())
          .withServiceType(requestedCriteria.getServiceType())
          .withVersion(pendingVersion)
        correlationId -> ConnectionCriteria.Initial.withRequested(criteria)
    }(collection.breakOut)

    val nextChildConnectionStates = initialReserveAlgorithm.nextSegments.map {
      case (correlationId, _) => correlationId -> CheckingReservationState
    }

    copy(
      childConnectionCriteria = childConnectionCriteria ++ nextChildConnectionCriteria,
      childConnectionStates = childConnectionStates ++ nextChildConnectionStates)
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
    else if (childConnectionStates.values.forall(Set(HeldReservationState, TimeoutReservationState))) TimeoutReservationState
    else if (childConnectionStates.values.forall(_ == ReservedReservationState)) ReservedReservationState
    else if (childConnectionStates.values.forall(_ == AbortedReservationState)) AbortedReservationState
    else throw new IllegalStateException(s"cannot determine aggregated state from child states ${childConnectionStates.values.mkString(",")}")

  def startProcessingNewCommand(command: NsiProviderMessage[NsiProviderOperation], transitionalState: ReservationState, children: ChildConnectionIds) = {
    // Skip aborted state when we never received a child connection id due to an immediate service exception.
    val stateForChildConnectionsWithoutConnectionId = if (transitionalState == AbortingReservationState) AbortedReservationState else transitionalState
    copy(
      command = command,
      childConnectionStates = childConnectionStates.map {
        case (correlationId, _) =>
          correlationId -> (if (children hasConnectionId correlationId) transitionalState else stateForChildConnectionsWithoutConnectionId)
      },
      childExceptions = Map.empty,
      childReserveTimeouts = Vector.empty,
      childResponses = Map.empty)
  }

  def childHasState(initialCorrelationId: CorrelationId, state: ReservationState): Boolean = {
    childConnectionStates.get(initialCorrelationId).exists(_ == state)
  }

  def childHasState(children: ChildConnectionIds, connectionId: ConnectionId, state: ReservationState): Boolean = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"missing child connection id for $connectionId"))
    childHasState(correlationId, state)
  }

  def updateChild(initialCorrelationId: CorrelationId, reservationState: ReservationState, exception: Option[ServiceExceptionType]): ReservationStateMachineData = {
    copy(
      childConnectionStates = childConnectionStates.updated(initialCorrelationId, reservationState),
      childExceptions = exception.fold(childExceptions - initialCorrelationId)(exception => childExceptions.updated(initialCorrelationId, exception)))
  }

  def updateChild(initialCorrelationId: CorrelationId, message: NsiRequesterMessage[NsiRequesterUpdate], reservationState: ReservationState, exception: Option[ServiceExceptionType] = None): ReservationStateMachineData = {
    val responses = message match {
      case message @ NsiRequesterMessage(_, reply: NsiCommandReply) =>
        childResponses.updated(initialCorrelationId, message.copy(body = reply))
      case _ =>
        childResponses
    }
    copy(
      childConnectionStates = childConnectionStates.updated(initialCorrelationId, reservationState),
      childExceptions = exception.fold(childExceptions - initialCorrelationId)(exception => childExceptions.updated(initialCorrelationId, exception)),
      childResponses = responses)
  }

  def updateChildByConnectionId(children: ChildConnectionIds, message: NsiRequesterMessage[NsiRequesterUpdate], reservationState: ReservationState, exception: Option[ServiceExceptionType] = None): ReservationStateMachineData = {
    val correlationId = children.initialCorrelationIdByConnectionId.getOrElse(message.body.connectionId, throw new IllegalStateException(s"missing child connection id for ${message.body.connectionId}"))
    updateChild(correlationId, message, reservationState, exception)
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
      childConnectionCriteria(segments.head._1).confirmed.get.pointToPointService.get.getSourceSTP,
      childConnectionCriteria(segments.last._1).confirmed.get.pointToPointService.get.getDestSTP)).toOption
  }

  def requestedCriteriaToHeld = copy(criteria = requestedToConfirmCriteria.fold(criteria.abort)(criteria.withHeld))

  def commitPendingCriteria = copy(criteria = criteria.commit)

  def pendingVersion = criteria.pendingVersion

  def processReserveTimeouts(children: ChildConnectionIds) = aggregatedReservationState match {
    case HeldReservationState => childReserveTimeouts.foldLeft(this)((acc, timeout) => acc.updateChildByConnectionId(children, timeout, TimeoutReservationState))
    case _ => this
  }

  def aggregatePathTrace(aggregatorNsa: String, connectionId: ConnectionId): ReservationStateMachineData = command match {
    case NsiProviderMessage(headers, _: InitialReserve) =>
      val pathTrace = headers.pathTrace.map { original =>
        new PathTraceType().withId(original.getId()).withConnectionId(original.getConnectionId())
      } getOrElse {
        new PathTraceType().withId(aggregatorNsa).withConnectionId(connectionId)
      }

      def extractProviderSegment(pathTrace: PathTraceType): Option[Vector[SegmentType]] = {
        pathTrace.getPath.asScala.headOption.map(_.getSegment.asScala.to[Vector])
      }

      val pathTraceSegments = segments.flatMap {
        case (correlationId, _) =>
          for {
            reply <- childResponses.get(correlationId).to[Vector]
            segment <- reply.headers.pathTrace.flatMap(extractProviderSegment) getOrElse {
              Vector(new SegmentType().withId(reply.headers.providerNSA).withConnectionId(reply.body.connectionId))
            }
          } yield segment
      }.zipWithIndex.map {
        case (segment, order) =>
          new SegmentType().withId(segment.getId()).withConnectionId(segment.getConnectionId()).withOrder(order).withStp(segment.getStp())
      }

      if (pathTraceSegments.isEmpty) {
        this
      } else {
        copy(aggregatedPathTrace = Some(pathTrace.withPath(new PathType().withSegment(pathTraceSegments.asJava))))
      }
    case _ =>
      this
  }
}

class ReservationStateMachine(
  aggregatorNsa: String,
  id: ConnectionId,
  initialReserve: NsiProviderMessage[InitialReserve],
  pceReplyUri: URI,
  children: => ChildConnectionIds,
  newCorrelationId: () => CorrelationId,
  newRequestHeaders: (NsiProviderMessage[NsiProviderOperation], ProviderEndPoint) => NsiHeaders,
  newNotificationId: () => Int,
  newNotifyHeaders: () => NsiHeaders,
  pathComputationAlgorithm: => PathComputationAlgorithm,
  failed: NsiError => GenericFailedType)
    extends FiniteStateMachine[ReservationState, ReservationStateMachineData, InboundMessage, OutboundMessage](
      InitialReservationState,
      ReservationStateMachineData(
        initialReserve,
        Option(initialReserve.body.body.getGlobalReservationId()).map(URI.create(_)),
        Option(initialReserve.body.body.getDescription()),
        ConnectionCriteria.Initial,
        InitialReserveAlgorithm.forAlgorithm(pathComputationAlgorithm))) {

  private def newInitialReserveNsiHeaders(provider: ProviderEndPoint) = {
    val pathTrace = initialReserve.headers.pathTrace getOrElse {
      new PathTraceType().withId(aggregatorNsa).withConnectionId(id)
    }
    newRequestHeaders(initialReserve, provider)
      .addConnectionTrace(s"$aggregatorNsa:$id")
      .withPathTrace(pathTrace)
  }

  when(InitialReservationState) {
    case Event(FromRequester(NsiProviderMessage(_, message: InitialReserve)), data) =>
      goto(PathComputationState) using data.copy(criteria = data.criteria.withRequested(message.criteria))
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data) =>
      goto(CheckingReservationState) using data.receivedSegments(pathComputationAlgorithm, children.segments).reserveNextSegments
    case Event(FromPce(message: PathComputationFailed), data) =>
      goto(FailedReservationState) using data.copy(pceError = Some(message.error))
    case Event(AckFromPce(failure: PceFailed), data) =>
      goto(FailedReservationState) using data.copy(pceError = Some(NsiError.TopologyError.copy(text = s"PCE failed to accept request ${failure.status} (${failure.statusText})")))
    case Event(AckFromPce(_: PceAccepted), _) =>
      stay
  }

  when(CheckingReservationState) {
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message: ReserveConfirmed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, reply.copy(body = message), HeldReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.withHeld(message.criteria))
        .initialReserveConfirmed(headers.correlationId, message.criteria)
        .reserveNextSegments
      val newData2 = newData.aggregatedReservationState match {
        case HeldReservationState => newData.requestedCriteriaToHeld.aggregatePathTrace(aggregatorNsa, id)
        case _                    => newData
      }
      val newData3 = newData2.processReserveTimeouts(children)
      goto(newData3.aggregatedReservationState) using newData3
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, reply.copy(body = message), FailedReservationState, Some(message.failed.getServiceException()))
        .modifyChildCriteria(children, message.connectionId)(_.abort)
        .clearNextSegments
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(message @ NsiRequesterMessage(headers, timeout: ReserveTimeout)), data) =>
      val newData = data.copy(childReserveTimeouts = data.childReserveTimeouts :+ message.copy(body = timeout))
      goto(newData.aggregatedReservationState) using newData
    case Event(AckFromProvider(NsiProviderMessage(headers, ServiceException(serviceException))), data) if data.childHasState(headers.correlationId, CheckingReservationState) =>
      val newData = data
        .updateChild(headers.correlationId, FailedReservationState, Some(serviceException))
        .modifyChildCriteria(headers.correlationId)(_.abort)
        .clearNextSegments
      goto(newData.aggregatedReservationState) using newData
    case Event(AckFromProvider(_), data) =>
      stay using data.clearNextSegments
  }

  when(HeldReservationState, TimeoutReservationState) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveCommit)), data) =>
      val newData = data.startProcessingNewCommand(message, CommittingReservationState, children)
      goto(CommittingReservationState) using newData
    case Event(FromRequester(message @ NsiProviderMessage(_, _: ReserveAbort)), data) =>
      val newData = data.startProcessingNewCommand(message, AbortingReservationState, children).copy(criteria = data.criteria.abort)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(message @ NsiRequesterMessage(headers, timeout: ReserveTimeout)), data) =>
      val newData = data
        .copy(childReserveTimeouts = data.childReserveTimeouts :+ message.copy(body = timeout))
        .processReserveTimeouts(children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommittingReservationState) {
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message: ReserveCommitConfirmed)), data) if data.childHasState(children, message.connectionId, CommittingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, reply.copy(body = message), ReservedReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.commit)
      val newData2 = if (newData.aggregatedReservationState == ReservedReservationState) newData.commitPendingCriteria else newData
      goto(newData2.aggregatedReservationState) using newData2
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message: ReserveCommitFailed)), data) if data.childHasState(children, message.connectionId, CommittingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, reply.copy(body = message), CommitFailedReservationState, Some(message.failed.getServiceException()))
        .modifyChildCriteria(children, message.connectionId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(AbortingReservationState) {
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message: ReserveAbortConfirmed)), data) if data.childHasState(children, message.connectionId, AbortingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, reply.copy(body = message), if (data.criteria.committed.isDefined) ReservedReservationState else AbortedReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(FailedReservationState) {
    case Event(FromRequester(command @ NsiProviderMessage(_, ReserveAbort(_))), data) =>
      val newData =
        if (data.reserveError.isDefined)
          data.copy(command = command, reserveError = None, criteria = data.criteria.abort)
        else
          data.startProcessingNewCommand(command, AbortingReservationState, children).copy(criteria = data.criteria.abort)
      goto(newData.aggregatedReservationState) using newData
  }

  when(ReservedReservationState) {
    case Event(FromRequester(command @ NsiProviderMessage(headers, ModifyReserve(reserve))), data) =>
      val pendingCriteria = reserve.getCriteria

      val error = validateModify(pendingCriteria, data.criteria.committed.get)

      val newData = error.map { error =>
        data.copy(command = command, childExceptions = Map.empty, reserveError = Some(error))
      }.getOrElse {
        val data2 = data
          .startProcessingNewCommand(command, ModifyingReservationState, children)
          .copy(criteria = data.criteria.withRequested(pendingCriteria))

        data2.copy(
          childConnectionCriteria = data.segments.map {
            case (initialCorrelationId, segment) =>
              val childCriteria = data.childConnectionCriteria(initialCorrelationId)

              val criteria = new ReservationRequestCriteriaType()
                .withSchedule(pendingCriteria.getSchedule())
                .withVersion(data2.pendingVersion)
              criteria.getAny().addAll(pendingCriteria.getAny())
              criteria.getOtherAttributes().putAll(pendingCriteria.getOtherAttributes())

              initialCorrelationId -> childCriteria.withRequested(criteria)
          }(collection.breakOut))
      }

      goto(newData.aggregatedReservationState) using newData
  }

  when(ModifyingReservationState) {
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message @ ReserveConfirmed(connectionId, _))), data) if data.childHasState(children, connectionId, ModifyingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, reply.copy(body = message), HeldReservationState)
        .modifyChildCriteria(children, message.connectionId)(_.withHeld(message.criteria))
      val newData2 = if (newData.aggregatedReservationState == HeldReservationState) newData.requestedCriteriaToHeld else newData
      goto(newData2.aggregatedReservationState) using newData2
    case Event(FromProvider(reply @ NsiRequesterMessage(headers, message: ReserveFailed)), data) if data.childHasState(children, message.connectionId, ModifyingReservationState) =>
      val newData = data
        .updateChildByConnectionId(children, reply.copy(body = message), FailedReservationState, Some(message.failed.getServiceException()))
        .modifyChildCriteria(children, message.connectionId)(_.abort)
      goto(newData.aggregatedReservationState) using newData
    case Event(FromProvider(notification @ NsiRequesterMessage(headers, message: ReserveTimeout)), data) =>
      val newData = data
        .copy(childReserveTimeouts = data.childReserveTimeouts :+ notification.copy(body = message))
        .processReserveTimeouts(children)
      goto(newData.aggregatedReservationState) using newData
  }

  when(CommitFailedReservationState)(PartialFunction.empty)
  when(AbortedReservationState)(PartialFunction.empty)

  whenUnhandled {
    case Event(AckFromProvider(_), _) => stay
    case Event(FromProvider(notification @ NsiRequesterMessage(_, message: ReserveTimeout)), data) =>
      stay using data
        .copy(childReserveTimeouts = data.childReserveTimeouts :+ notification.copy(body = message))
        .processReserveTimeouts(children)
  }

  onTransition(({
    case InitialReservationState -> PathComputationState =>
      val criteria = nextStateData.criteria.requested.get
      Seq(ToPce(PathComputationRequest(
        newCorrelationId(),
        pceReplyUri,
        criteria.schedule.flatMap(_.startTime.toOption(None)),
        criteria.schedule.flatMap(_.endTime.toOption(None)),
        ServiceType(criteria.getServiceType(), criteria.pointToPointService.get),
        pathComputationAlgorithm,
        initialReserve.headers.connectionTrace
      )))

    case (PathComputationState | CheckingReservationState) -> CheckingReservationState =>
      val data = nextStateData
      val nextSegments = data.initialReserveAlgorithm.nextSegments.map {
        case (correlationId, segment) =>
          val criteria = nextStateData.childConnectionCriteria(correlationId)

          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.map(_.toASCIIString()).orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria.requested.get)

          val headers = newInitialReserveNsiHeaders(segment.provider).copy(correlationId = correlationId)

          ToProvider(NsiProviderMessage(headers, InitialReserve(reserveType)), segment.provider)
      }(collection.breakOut)

      nextSegments
    case PathComputationState -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.pceError.getOrElse(NsiError.NoServicePlanePathFound))))
    case (CheckingReservationState | ModifyingReservationState) -> FailedReservationState =>
      val baseError = failed(NsiError.GenericInternalError.withText("reservation failed without child errors"))

      val childExceptions = nextStateData.childExceptions.values.to[Vector]
      baseError.getServiceException.withChildException(childExceptions.asJava)

      val prioritizedChildError = childExceptions.headOption
      prioritizedChildError foreach { error =>
        baseError.getServiceException()
          .withErrorId(error.getErrorId)
          .withText(error.getText)
          .withVariables(error.getVariables)
      }

      respond(ReserveFailed(baseError))
    case CheckingReservationState -> (HeldReservationState | TimeoutReservationState) =>
      val replyHeaders = nextStateData.aggregatedPathTrace.foldLeft(nextStateData.command.headers.forAsyncReply)(_ withPathTrace _)
      Seq(ToRequester(NsiRequesterMessage(replyHeaders, ReserveConfirmed(id, nextStateData.criteria.confirmed.get))))
    case ModifyingReservationState -> (HeldReservationState | TimeoutReservationState) =>
      respond(ReserveConfirmed(id, nextStateData.criteria.confirmed.get))
    case HeldReservationState -> CommittingReservationState =>
      val completedPathTrace = if (nextStateData.criteria.committed.isEmpty && initialReserve.headers.pathTrace.isEmpty) nextStateData.aggregatedPathTrace else nextStateData.command.headers.pathTrace
      children.childConnections.collect {
        case (seg, _, Present(connectionId)) =>
          val headers = newRequestHeaders(nextStateData.command, seg.provider)
          ToProvider(NsiProviderMessage(completedPathTrace.foldLeft(headers)(_ withPathTrace _), ReserveCommit(connectionId)), seg.provider)
      }.toVector
    case (HeldReservationState | FailedReservationState) -> AbortingReservationState =>
      children.childConnections.collect {
        case (seg, _, Present(connectionId)) =>
          ToProvider(NsiProviderMessage(newRequestHeaders(nextStateData.command, seg.provider), ReserveAbort(connectionId)), seg.provider)
      }.toVector
    case CommittingReservationState -> ReservedReservationState =>
      respond(ReserveCommitConfirmed(id))
    case CommittingReservationState -> CommitFailedReservationState =>
      respond(ReserveCommitFailed(failed(NsiError.GenericInternalError).tap(_.getServiceException().withChildException(stateData.childExceptions.values.toSeq.asJava))))
    case ReservedReservationState -> ModifyingReservationState =>
      val data = nextStateData
      children.childConnections.collect {
        case (segment, initialCorrelationId, Present(childConnectionId)) =>
          val criteria = nextStateData.childConnectionCriteria(initialCorrelationId)

          val reserveType = new ReserveType()
            .withConnectionId(childConnectionId)
            .withCriteria(criteria.requested.get)

          val headers = newRequestHeaders(data.command, segment.provider)

          ToProvider(NsiProviderMessage(headers, ModifyReserve(reserveType)), segment.provider)
      }
    case (InitialReservationState | ReservedReservationState) -> FailedReservationState =>
      respond(ReserveFailed(failed(nextStateData.reserveError.get)))
    case (HeldReservationState | FailedReservationState | AbortingReservationState) -> (AbortedReservationState | ReservedReservationState) =>
      respond(ReserveAbortConfirmed(id))
    case _ =>
      // Ensure the andThen notify reserve timeout below is called
      Vector.empty
  }: TransitionHandler).andThen(_ ++ notifyReserveTimeout))

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

  private def respond(body: NsiRequesterOperation) = Seq(ToRequester(nextStateData.command reply body))

  private def notifyReserveTimeout = nextStateData.childReserveTimeouts.filterNot(
    x => stateData.childReserveTimeouts.exists(_.body.connectionId == x.body.connectionId)
  ).map(message =>
    ToRequester(
      NsiRequesterMessage(
        newNotifyHeaders(),
        ReserveTimeout(
          new ReserveTimeoutRequestType()
            .withConnectionId(id)
            .withNotificationId(newNotificationId())
            .withTimeStamp(message.body.notification.getTimeStamp())
            .withTimeoutValue(message.body.notification.getTimeoutValue())
            .withOriginatingConnectionId(message.body.notification.getOriginatingConnectionId())
            .withOriginatingNSA(message.body.notification.getOriginatingNSA())
        )
      )
    )
  )

  private def validateModify(requestedCriteria: ReservationRequestCriteriaType, committedCriteria: ReservationConfirmCriteriaType) = {
    val requestedVersion: Int = if (requestedCriteria.getVersion ne null) requestedCriteria.getVersion else 0
    if (committedVersion >= requestedVersion)
      Some(NsiError.GenericMessagePayloadError.copy(text = s"requested version ${requestedVersion} must be greater than committed version ${committedVersion}"))
    else if (requestedCriteria.modifiedParameters.exists(_.getType == PATH_COMPUTATION_ALGORITHM_PARAMETER_TYPE))
      Some(NsiError.GenericMessagePayloadError.copy(text = s"parameter ${PATH_COMPUTATION_ALGORITHM_PARAMETER_TYPE} is not modifiable"))
    else
      None
  }
}
