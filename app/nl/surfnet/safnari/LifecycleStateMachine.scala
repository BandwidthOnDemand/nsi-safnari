package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType

case class LifecycleStateMachineData(children: Map[ConnectionId, ProviderEndPoint], childConnectionStates: Map[ConnectionId, LifecycleStateEnumType], commandHeaders: Option[NsiHeaders] = None) {

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

class LifecycleStateMachine(connectionId: ConnectionId, newNsiHeaders: ProviderEndPoint => NsiHeaders, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[LifecycleStateEnumType, LifecycleStateMachineData, InboundMessage, OutboundMessage](CREATED, new LifecycleStateMachineData(children, children.map(_._1 -> CREATED))) {

  when(CREATED) {
    case Event(FromRequester(message: Terminate), data) =>
      goto(TERMINATING) using (data.copy(commandHeaders = Some(message.headers)))
  }

  when(TERMINATING) {
    case Event(FromProvider(message: TerminateConfirmed), data) if data.childHasState(message.connectionId, CREATED) =>
      val newData = data.updateChild(message.connectionId, TERMINATED)
      goto(newData.aggregatedLifecycleStatus) using (newData)
  }

  when(TERMINATED)(PartialFunction.empty)

  onTransition {
    case CREATED -> TERMINATING =>
      stateData.children.map {
        case (connectionId, provider) =>
          ToProvider(Terminate(newNsiHeaders(provider), connectionId), provider)
      }.toVector
    case TERMINATING -> TERMINATED =>
      Seq(ToRequester(TerminateConfirmed(stateData.commandHeaders.get.asReply, connectionId)))
  }

  def lifecycleState = stateName
  def childConnectionState(connectionId: ConnectionId) = stateData.childConnectionStates(connectionId)
}
