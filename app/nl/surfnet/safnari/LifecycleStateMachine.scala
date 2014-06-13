package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

case class LifecycleStateMachineData(
  childConnectionStates: Map[ConnectionId, LifecycleStateEnumType] = Map.empty,
  command: Option[NsiProviderMessage[NsiProviderOperation]] = None,
  errorEvent: Option[ErrorEvent] = None) {

  def aggregatedLifecycleStatus: LifecycleStateEnumType = {
    if (childConnectionStates.values.forall(_ == CREATED)) CREATED
    else if (childConnectionStates.values.exists(_ == TERMINATING)) TERMINATING
    else if (childConnectionStates.values.forall(_ == TERMINATED)) TERMINATED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childConnectionStates.values}")
  }

  def startCommand(command: NsiProviderMessage[NsiProviderOperation], transitionalState: LifecycleStateEnumType, children: ChildConnectionIds) =
    copy(command = Some(command), childConnectionStates = children.childrenByConnectionId.map(_._1 -> transitionalState))

  def updateChild(connectionId: ConnectionId, state: LifecycleStateEnumType) =
    copy(childConnectionStates = childConnectionStates.updated(connectionId, state))

  def childHasState(connectionId: ConnectionId, state: LifecycleStateEnumType) =
    childConnectionStates.getOrElse(connectionId, CREATED) == state
}

class LifecycleStateMachine(connectionId: ConnectionId, newNsiHeaders: ProviderEndPoint => NsiHeaders, newNotifyHeaders: () => NsiHeaders, newNotificationId: () => Int, children: => ChildConnectionIds)
  extends FiniteStateMachine[LifecycleStateEnumType, LifecycleStateMachineData, InboundMessage, OutboundMessage](CREATED, LifecycleStateMachineData()) {

  when(CREATED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using data.startCommand(message, TERMINATING, children)
    case Event(FromProvider(NsiRequesterMessage(_, errorEvent: ErrorEvent)), data) if errorEvent.error.getEvent() == EventEnumType.FORCED_END =>
      goto(FAILED) using data.updateChild(errorEvent.connectionId, FAILED).copy(errorEvent = Some(errorEvent))
    case Event(PassedEndTime(_, _, _), data) =>
      goto(PASSED_END_TIME)
  }

  when(TERMINATING) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: TerminateConfirmed)), data) if data.childHasState(message.connectionId, TERMINATING) =>
      val newData = data.updateChild(message.connectionId, TERMINATED)
      goto(newData.aggregatedLifecycleStatus) using (newData)
  }

  when(TERMINATED)(PartialFunction.empty)

  when(FAILED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using (data.copy(command = Some(message)))
  }

  when(PASSED_END_TIME) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using (data.copy(command = Some(message)))
  }

  whenUnhandled {
    case Event(AckFromProvider(NsiProviderMessage(headers, ReserveResponse(connectionId))), data) =>
      stay using data.updateChild(connectionId, CREATED)
    case Event(FromProvider(NsiRequesterMessage(headers, body: ReserveConfirmed)), data) =>
      stay using data.updateChild(body.connectionId, CREATED)
    case Event(FromProvider(NsiRequesterMessage(headers, body: ReserveFailed)), data) =>
      stay using data.updateChild(body.connectionId, CREATED)
    case Event(AckFromProvider(_), _)     => stay
    case Event(PassedEndTime(_, _, _), _) => stay
  }

  onTransition {
    case (CREATED | FAILED | PASSED_END_TIME) -> TERMINATING =>
      children.childrenByConnectionId.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(provider), Terminate(connectionId)), provider)
      }.toVector
    case TERMINATING -> TERMINATED =>
      Seq(ToRequester(stateData.command.get reply TerminateConfirmed(connectionId)))
    case CREATED -> FAILED =>
      val original = nextStateData.errorEvent.get
      val headers = newNotifyHeaders()
      val event = ErrorEvent(new ErrorEventType()
        .withConnectionId(connectionId)
        .withNotificationId(newNotificationId())
        .withTimeStamp(original.error.getTimeStamp())
        .withEvent(EventEnumType.FORCED_END)
        .withOriginatingConnectionId(original.error.getOriginatingConnectionId())
        .withOriginatingNSA(original.error.getOriginatingNSA())
        .withAdditionalInfo(original.error.getAdditionalInfo()))
      if (original.error.getServiceException() ne null) {
        event.error.withServiceException(new ServiceExceptionType()
          .withConnectionId(connectionId)
          .withErrorId(original.error.getServiceException().getErrorId())
          .withText(original.error.getServiceException().getText())
          .withNsaId(headers.providerNSA)
          .withServiceType(original.error.getServiceException().getServiceType())
          .withChildException(original.error.getServiceException()))
      }
      Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event)))
    case CREATED -> PASSED_END_TIME =>
      Seq.empty
  }

  def lifecycleState = stateName
  def childConnectionState(connectionId: ConnectionId) = stateData.childConnectionStates.getOrElse(connectionId, CREATED)
}
