package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.DataPlaneStatusType
import javax.xml.datatype.XMLGregorianCalendar

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, Boolean], timeStamp: Option[XMLGregorianCalendar]) {

  def initialize(providers: Map[ConnectionId, ProviderEndPoint]) =
    DataPlaneStateMachineData(providers, providers.keys.map(_ -> false).toMap, None)

  def aggregatedProvisionStatus: Boolean = childStates.values.reduce(_ && _)

  def updateState(connectionId: ConnectionId, state: Boolean, newTimeStamp: XMLGregorianCalendar) = {
    val latestTimeStamp = timeStamp.map(implicitly[Ordering[XMLGregorianCalendar]].max(_, newTimeStamp)).orElse(Some(newTimeStamp))
    copy(childStates = childStates + (connectionId -> state), timeStamp = latestTimeStamp)
  }
}

class DataPlaneStateMachine(connectionId: ConnectionId, newNsiHeaders: () => NsiHeaders) extends FiniteStateMachine(false, DataPlaneStateMachineData(Map.empty, Map.empty, None)) {

  when(false) {
    case Event(downstreamConnections: Map[_, _], data) =>
      stay using data.initialize(downstreamConnections.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ComputedSegment].provider))
  }

  when(true)(PartialFunction.empty)

  whenUnhandled {
    case Event(FromProvider(message: DataPlaneStateChange), data) =>
      val newData = data.updateState(message.connectionId, message.status.isActive(), message.timeStamp)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.headers.asReply)
  }

  onTransition {
    case false -> true | true -> false =>
      Seq(ToRequester(DataPlaneStateChange(
        newNsiHeaders(),
        connectionId,
        new DataPlaneStatusType().withVersion(0).withActive(nextStateName).withVersionConsistent(true), nextStateData.timeStamp.get)))
  }

  def dataPlaneStatus(version: Int) = new DataPlaneStatusType().withVersion(version).withActive(stateName).withVersionConsistent(true)

  def childConnectionState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
