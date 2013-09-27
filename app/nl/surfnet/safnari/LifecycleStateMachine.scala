package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.EventEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.ErrorEventType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

case class LifecycleStateMachineData(
    children: Map[ConnectionId, ProviderEndPoint],
    childConnectionStates: Map[ConnectionId, LifecycleStateEnumType],
    command: Option[NsiProviderMessage[NsiProviderOperation]] = None,
    errorEvent: Option[ErrorEvent] = None) {

  def aggregatedLifecycleStatus: LifecycleStateEnumType = {
    if (childConnectionStates.values.forall(_ == CREATED)) CREATED
    else if (childConnectionStates.values.exists(_ == TERMINATING)) TERMINATING
    else if (childConnectionStates.values.forall(_ == TERMINATED)) TERMINATED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childConnectionStates.values}")
  }

  def updateChild(connectionId: ConnectionId, state: LifecycleStateEnumType) =
    copy(childConnectionStates = childConnectionStates.updated(connectionId, state))

  def childHasState(connectionId: ConnectionId, state: LifecycleStateEnumType) =
    childConnectionStates.getOrElse(connectionId, CREATED) == state
}

class LifecycleStateMachine(connectionId: ConnectionId, newNsiHeaders: ProviderEndPoint => NsiHeaders, newNotifyHeaders: () => NsiHeaders, newNotificationId: () => Int, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[LifecycleStateEnumType, LifecycleStateMachineData, InboundMessage, OutboundMessage](CREATED, new LifecycleStateMachineData(children, children.map(_._1 -> CREATED))) {

  when(CREATED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using (data.copy(command = Some(message)))
  }

  when(TERMINATING) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: TerminateConfirmed)), data) if data.childHasState(message.connectionId, CREATED) =>
      val newData = data.updateChild(message.connectionId, TERMINATED)
      goto(newData.aggregatedLifecycleStatus) using (newData)
  }

  when(TERMINATED)(PartialFunction.empty)

  when(FAILED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Terminate)), data) =>
      goto(TERMINATING) using (data.copy(command = Some(message)))
  }

  whenUnhandled {
    case Event(FromProvider(NsiRequesterMessage(_, errorEvent: ErrorEvent)), data) if errorEvent.error.getEvent() == EventEnumType.FORCED_END =>
      goto(FAILED) using data.updateChild(errorEvent.connectionId, FAILED).copy(errorEvent = Some(errorEvent))
  }

  onTransition {
    case _ -> TERMINATING =>
      stateData.children.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(provider), Terminate(connectionId)), provider)
      }.toVector
    case TERMINATING -> TERMINATED =>
      Seq(ToRequester(stateData.command.get reply TerminateConfirmed(connectionId)))
    case _ -> FAILED =>
      val original = nextStateData.errorEvent.get
      val headers = newNotifyHeaders()
      val event = ErrorEvent(new ErrorEventType()
        .withConnectionId(connectionId)
        .withNotificationId(newNotificationId())
        .withTimeStamp(original.error.getTimeStamp())
        .withEvent(EventEnumType.FORCED_END)
        .withServiceException(new ServiceExceptionType()
          .withConnectionId(connectionId)
          // TODO error id?
          .withErrorId(original.error.getServiceException().getErrorId())
          // TODO error text?
          .withText(original.error.getServiceException().getText())
          .withNsaId(headers.providerNSA)
          // TODO service type?
          .withServiceType(original.error.getServiceException().getServiceType())
          .withChildException(original.error.getServiceException())))
      Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event)))
  }

  def lifecycleState = stateName
  def childConnectionState(connectionId: ConnectionId) = stateData.childConnectionStates(connectionId)
}
