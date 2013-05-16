package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateType


case class LifecycleStateMachineData(children: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, LifecycleStateEnumType], correlationId: Option[CorrelationId] = None) {

  def initialize(children: Map[ConnectionId, ProviderEndPoint]) = {
    LifecycleStateMachineData(children, children.keys.map(_ -> INITIAL).toMap)
  }

  def aggregatedLifecycleStatus: LifecycleStateEnumType = {
    if (childStates.values.exists(_ == UNKNOWN)) UNKNOWN
    else if (childStates.values.forall(_ == INITIAL)) INITIAL
    else if (childStates.values.exists(_ == TERMINATING)) TERMINATING
    else if (childStates.values.forall(_ == TERMINATED)) TERMINATED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childStates.values}")
  }

  def updateChild(connectionId: ConnectionId, state: LifecycleStateEnumType) =
    copy(childStates = childStates.updated(connectionId, state))

  def childHasState(connectionId: ConnectionId, state: LifecycleStateEnumType) =
    childStates.getOrElse(connectionId, UNKNOWN) == state
}

class LifecycleStateMachine(connectionId: ConnectionId, newCorrelationId: () => CorrelationId, outbound: Message => Unit) extends FiniteStateMachine(INITIAL, new LifecycleStateMachineData(Map.empty, Map.empty)) {

  when(INITIAL) {
    case Event(children: Map[_, _], data) =>
      stay using data.initialize(children.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
    case Event(FromRequester(message: Terminate), data) =>
      goto(TERMINATING) using(data.copy(correlationId = Some(message.correlationId))) replying GenericAck(message.correlationId)
  }

  when(TERMINATING) {
    case Event(FromProvider(message: TerminateConfirmed), data) if data.childHasState(message.connectionId, INITIAL)=>
      val newData = data.updateChild(message.connectionId, TERMINATED)
      goto(newData.aggregatedLifecycleStatus) using(newData) replying GenericAck(message.correlationId)
  }

  when(TERMINATED)(PartialFunction.empty)

  onTransition {
    case INITIAL -> TERMINATING =>
      stateData.children.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Terminate(newCorrelationId(), connectionId), provider))
      }
    case TERMINATING -> TERMINATED =>
      outbound(ToRequester(TerminateConfirmed(stateData.correlationId.get, connectionId)))
  }

  def lifecycleState = new LifecycleStateType().withState(stateName)
}
