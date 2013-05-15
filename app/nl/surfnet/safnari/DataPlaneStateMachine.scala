package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.DataPlaneStatusType
import javax.xml.datatype.XMLGregorianCalendar

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], states: Map[ConnectionId, Boolean], timeStamp: Option[XMLGregorianCalendar]) {

  def initialize(providers: Map[ConnectionId, ProviderEndPoint]) =
    DataPlaneStateMachineData(providers, providers.keys.map(_ -> false).toMap, None)

  def aggregatedProvisionStatus: Boolean = states.values.reduce(_ && _)

  def updateState(connectionId: ConnectionId, state: Boolean, newTimeStamp: XMLGregorianCalendar) = {
    val latestTimeStamp = timeStamp.map(implicitly[Ordering[XMLGregorianCalendar]].max(_, newTimeStamp)).orElse(Some(newTimeStamp))
    copy(states = states + (connectionId -> state), timeStamp = latestTimeStamp)
  }
}

class DataPlaneStateMachine(connectionId: ConnectionId, newCorrelationId: () => CorrelationId, outbound: Message => Unit) extends FiniteStateMachine(false, DataPlaneStateMachineData(Map.empty, Map.empty, None)) {

  when(false) {
    case Event(downstreamConnections: Map[_, _], data) =>
      stay using data.initialize(downstreamConnections.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
  }

  when(true)(PartialFunction.empty)

  whenUnhandled {
    case Event(FromProvider(message: DataPlaneStateChange), data) =>
      val newData = data.updateState(message.connectionId, message.status.isActive(), message.timeStamp)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.correlationId)
  }

  onTransition {
    case false -> true | true -> false =>
      outbound(ToRequester(DataPlaneStateChange(
        newCorrelationId(),
        connectionId,
        new DataPlaneStatusType().withVersion(0).withActive(nextStateName).withVersionConsistent(true), nextStateData.timeStamp.get)))
  }

  def dataPlaneStatus(version: Int) = new DataPlaneStatusType().withVersion(version).withActive(stateName).withVersionConsistent(true)
}
