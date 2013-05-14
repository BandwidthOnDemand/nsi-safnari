package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.DataPlaneStatusType

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], states: Map[ConnectionId, Boolean]) {

  def initialize(providers: Map[ConnectionId, ProviderEndPoint]) =
    DataPlaneStateMachineData(providers, providers.keys.map(_ -> false).toMap)

  def aggregatedProvisionStatus: Boolean = states.values.reduce(_ && _)

  def updateState(connectionId: ConnectionId, state: Boolean) =
    copy(states = states + (connectionId -> state))
}

class DataPlaneStateMachine(connectionId: ConnectionId, newCorrelationId: () => CorrelationId, outbound: Message => Unit) extends FiniteStateMachine(false, DataPlaneStateMachineData(Map.empty, Map.empty)) {

  when(false) {
    case Event(downstreamConnections: Map[_, _], data) =>
      stay using data.initialize(downstreamConnections.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
  }

  when(true)(PartialFunction.empty)

  whenUnhandled {
    case Event(FromProvider(message: DataPlaneStateChanged), data) =>
      val newData = data.updateState(message.connectionId, message.status.isActive())
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.correlationId)
  }

  onTransition {
    case false -> true =>
      outbound(ToRequester(DataPlaneStateChanged(newCorrelationId(), connectionId, new DataPlaneStatusType().withVersion(0).withActive(true).withVersionConsistent(true))))
    case true -> false =>
      outbound(ToRequester(DataPlaneStateChanged(newCorrelationId(), connectionId, new DataPlaneStatusType().withVersion(0).withActive(false).withVersionConsistent(true))))
  }

  def dataPlaneStatus(version: Int) = new DataPlaneStatusType().withVersion(version).withActive(stateName).withVersionConsistent(true)
}
