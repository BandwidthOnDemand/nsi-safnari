package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.DataPlaneStatusType
import javax.xml.datatype.XMLGregorianCalendar

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, Boolean], timeStamp: Option[XMLGregorianCalendar]) {

  def aggregatedProvisionStatus: Boolean = childStates.values.reduce(_ && _)

  def updateState(connectionId: ConnectionId, state: Boolean, newTimeStamp: XMLGregorianCalendar) = {
    val latestTimeStamp = timeStamp.map(implicitly[Ordering[XMLGregorianCalendar]].max(_, newTimeStamp)).orElse(Some(newTimeStamp))
    copy(childStates = childStates + (connectionId -> state), timeStamp = latestTimeStamp)
  }
}

class DataPlaneStateMachine(connectionId: ConnectionId, newNsiHeaders: () => NsiHeaders, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[Boolean, DataPlaneStateMachineData, InboundMessage, OutboundMessage](false, DataPlaneStateMachineData(children, children.map(_._1 -> false), None)) {

  when(false)(PartialFunction.empty)
  when(true)(PartialFunction.empty)

  whenUnhandled {
    case Event(FromProvider(NsiRequesterMessage(headers, message: DataPlaneStateChange)), data) =>
      val newData = data.updateState(message.connectionId, message.status.isActive(), message.timeStamp)
      goto(newData.aggregatedProvisionStatus) using newData
  }

  onTransition {
    case false -> true | true -> false =>
      Seq(ToRequester(NsiRequesterMessage(newNsiHeaders(), DataPlaneStateChange(
        connectionId,
        new DataPlaneStatusType().withVersion(0).withActive(nextStateName).withVersionConsistent(true), nextStateData.timeStamp.get))))
  }

  def dataPlaneStatus(version: Int) = new DataPlaneStatusType().withVersion(version).withActive(stateName).withVersionConsistent(true)

  def childConnectionState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
