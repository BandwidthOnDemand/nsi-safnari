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

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._

import org.ogf.schemas.nsi._2013._12.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

case class LifecycleStateMachineData(
  childConnectionStates: Map[CorrelationId, LifecycleStateEnumType] = Map.empty,
  command: Option[NsiProviderMessage[NsiProviderOperation]] = None,
  errorEvent: Option[ErrorEvent] = None,
  sendTerminateRequest: Map[ConnectionId, ProviderEndPoint] = Map.empty) {

  def aggregatedLifecycleStatus: LifecycleStateEnumType = {
    if (childConnectionStates.values.forall(_ == CREATED)) CREATED
    else if (childConnectionStates.values.exists(_ == FAILED)) FAILED
    else if (childConnectionStates.values.exists(_ == TERMINATING)) TERMINATING
    else if (childConnectionStates.values.forall(_ == TERMINATED)) TERMINATED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childConnectionStates.values}")
  }

  def startTerminate(command: NsiProviderMessage[NsiProviderOperation], children: ChildConnectionIds) = {
    def transitionalState(id: CorrelationId) = children.connectionByInitialCorrelationId.get(id) match {
      case Some(Never) => TERMINATED
      case None => TERMINATED // Initial Reserve for this segment was not yet started
      case _ => TERMINATING
    }

    copy(command = Some(command), childConnectionStates = children.segments.map {
      case (correlationId, _) => correlationId -> transitionalState(correlationId)
    }.toMap)
  }

  def updateChild(correlationId: CorrelationId, state: LifecycleStateEnumType) =
    copy(childConnectionStates = childConnectionStates.updated(correlationId, state))

  def updateChild(children: ChildConnectionIds, connectionId: ConnectionId, state: LifecycleStateEnumType) =
    copy(childConnectionStates = childConnectionStates.updated(children.initialCorrelationIdFor(connectionId), state))

  def childHasState(correlationId: CorrelationId, state: LifecycleStateEnumType) =
    childConnectionStates.getOrElse(correlationId, CREATED) == state

  def childHasState(children: ChildConnectionIds, connectionId: ConnectionId, state: LifecycleStateEnumType) =
    childConnectionStates.getOrElse(children.initialCorrelationIdFor(connectionId), CREATED) == state
}

/**
 * Implementation of the NSI v2.0 Lifecycle State Machine. The implementation
 * is complicated by the fact that a `Terminate` request may be received
 * before all the child connection ids are known.
 *
 * To deal with this we keep a special map in the data that contains the
 * child connection ids that need a terminate request.
 */
class LifecycleStateMachine(
  connectionId: ConnectionId,
  newRequestHeaders: (NsiProviderMessage[NsiProviderOperation], ProviderEndPoint) => NsiHeaders,
  newNotifyHeaders: () => NsiHeaders,
  newNotificationId: () => Int,
  children: => ChildConnectionIds
) extends FiniteStateMachine[LifecycleStateEnumType, LifecycleStateMachineData, InboundMessage, OutboundMessage](CREATED, LifecycleStateMachineData()) {

  when(CREATED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) if children.childConnections.nonEmpty =>
      val newData = data.startTerminate(message, children)
        .copy(sendTerminateRequest = children.childrenByConnectionId)
      goto(newData.aggregatedLifecycleStatus) using newData
    case Event(FromProvider(NsiRequesterMessage(_, errorEvent: ErrorEvent)), data) if errorEvent.notification.getEvent() == EventEnumType.FORCED_END =>
      goto(FAILED) using data.updateChild(children, errorEvent.connectionId, FAILED).copy(errorEvent = Some(errorEvent))
    case Event(PassedEndTime(_, _, _), _) =>
      goto(PASSED_END_TIME)
  }

  when(TERMINATING) {
    case Event(FromProvider(NsiRequesterMessage(_, message: TerminateConfirmed)), data) if data.childHasState(children, message.connectionId, TERMINATING) =>
      val newData = data.updateChild(children, message.connectionId, TERMINATED).copy(sendTerminateRequest = Map.empty)
      goto(newData.aggregatedLifecycleStatus) using newData
    case Event(AckFromProvider(NsiProviderMessage(_, body: ReserveResponse)), data) =>
      stay using data.updateChild(children, body.connectionId, TERMINATING).copy(sendTerminateRequest = Map(body.connectionId -> children.childrenByConnectionId(connectionId)))
    case Event(FromProvider(NsiRequesterMessage(_, body: ReserveConfirmed)), data) =>
      stay using data.updateChild(children, body.connectionId, TERMINATING).copy(sendTerminateRequest = Map(body.connectionId -> children.childrenByConnectionId(body.connectionId)))
    case Event(FromProvider(NsiRequesterMessage(_, body: ReserveFailed)), data) =>
      stay using data.updateChild(children, body.connectionId, TERMINATING).copy(sendTerminateRequest = Map(body.connectionId -> children.childrenByConnectionId(body.connectionId)))
  }

  when(TERMINATED)(PartialFunction.empty)

  when(FAILED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using data.startTerminate(message, children).copy(sendTerminateRequest = children.childrenByConnectionId)
  }

  when(PASSED_END_TIME) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using data.startTerminate(message, children).copy(sendTerminateRequest = children.childrenByConnectionId)
  }

  whenUnhandled {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) if children.childConnections.isEmpty =>
      goto(TERMINATED) using data.copy(command = Some(message))
    case Event(AckFromProvider(NsiProviderMessage(_, ReserveResponse(connectionId))), data) =>
      stay using data.updateChild(children, connectionId, CREATED).copy(sendTerminateRequest = Map.empty)
    case Event(FromProvider(NsiRequesterMessage(_, body: ReserveConfirmed)), data) =>
      stay using data.updateChild(children, body.connectionId, CREATED).copy(sendTerminateRequest = Map.empty)
    case Event(FromProvider(NsiRequesterMessage(_, body: ReserveFailed)), data) =>
      stay using data.updateChild(children, body.connectionId, CREATED).copy(sendTerminateRequest = Map.empty)
    case Event(AckFromProvider(_), data) =>
      stay using data.copy(sendTerminateRequest = Map.empty)
    case Event(PassedEndTime(_, _, _), data) =>
      stay using data.copy(sendTerminateRequest = Map.empty)
  }

  onTransition {
    case (CREATED | FAILED | PASSED_END_TIME | TERMINATING) -> TERMINATING =>
      nextStateData.sendTerminateRequest.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newRequestHeaders(nextStateData.command.get, provider), Terminate(connectionId)), provider)
      }.toVector
    case (CREATED | FAILED | PASSED_END_TIME | TERMINATING) -> TERMINATED =>
      Seq(ToRequester(nextStateData.command.get reply TerminateConfirmed(connectionId)))
    case CREATED -> FAILED =>
      val original = nextStateData.errorEvent.get
      val headers = newNotifyHeaders()
      val event = ErrorEvent(new ErrorEventType()
        .withConnectionId(connectionId)
        .withNotificationId(newNotificationId())
        .withTimeStamp(original.notification.getTimeStamp())
        .withEvent(EventEnumType.FORCED_END)
        .withOriginatingConnectionId(original.notification.getOriginatingConnectionId())
        .withOriginatingNSA(original.notification.getOriginatingNSA())
        .withAdditionalInfo(original.notification.getAdditionalInfo()))
      if (original.notification.getServiceException() ne null) {
        event.notification.withServiceException(new ServiceExceptionType()
          .withConnectionId(connectionId)
          .withErrorId(original.notification.getServiceException().getErrorId())
          .withText(original.notification.getServiceException().getText())
          .withNsaId(headers.providerNSA)
          .withServiceType(original.notification.getServiceException().getServiceType())
          .withChildException(original.notification.getServiceException()))
      }
      Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event)))
    case CREATED -> PASSED_END_TIME =>
      Seq.empty
  }

  def lifecycleState = stateName
  def childConnectionState(connectionId: ConnectionId) =
    stateData.childConnectionStates.getOrElse(children.initialCorrelationIdFor(connectionId), CREATED)
}
