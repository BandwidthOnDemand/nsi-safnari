package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateType


case class LifecycleStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], states: Map[ConnectionId, LifecycleStateEnumType], correlationId: Option[CorrelationId] = None) {

  def initialize(providers: Map[ConnectionId, ProviderEndPoint]) = {
    LifecycleStateMachineData(providers, providers.keys.map(_ -> UNKNOWN).toMap)
  }

  def aggregatedLifecycleStatus: LifecycleStateEnumType = {
    if (states.values.exists(_ == UNKNOWN)) UNKNOWN
    else if (states.values.exists(_ == TERMINATING)) TERMINATING
    else if (states.values.forall(_ == INITIAL)) INITIAL
    else if (states.values.forall(_ == TERMINATED)) TERMINATED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${states.values}")
  }

  def updateState(connectionId: ConnectionId, state: LifecycleStateEnumType) = {
    copy(states = states + (connectionId -> state))
  }
}

class LifecycleStateMachine(connectionId: ConnectionId, newCorrelationId: () => CorrelationId, outbound: Message => Unit) extends FiniteStateMachine(INITIAL, new LifecycleStateMachineData(Map.empty, Map.empty)) {

  when(INITIAL) {
    case Event(downstreamConnections: Map[_, _], data) =>
      stay using data.initialize(downstreamConnections.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
    case Event(FromRequester(message: Terminate), data) => goto(TERMINATING) using(data.copy(correlationId = Some(message.correlationId))) replying GenericAck(message.correlationId)
  }

  when(TERMINATING) {
    case Event(FromProvider(message: TerminateConfirmed), data) =>
      val newData = data.updateState(message.connectionId, TERMINATED)
      goto(newData.aggregatedLifecycleStatus) using(newData) replying GenericAck(message.correlationId)
  }

  when(TERMINATED)(PartialFunction.empty)

  onTransition {
    case INITIAL -> TERMINATING =>
      stateData.providers.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Terminate(newCorrelationId(), connectionId), provider))
      }
    case TERMINATING -> TERMINATED =>
      outbound(ToRequester(TerminateConfirmed(stateData.correlationId.get, connectionId)))
  }

  def lifecycleState(version: Int) =
    new LifecycleStateType().withState(stateName).withVersion(version)
}
