package nl.surfnet.safnari

import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.utils._

import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.nsiv2.messages.{DataPlaneStateChange, NsiRequesterMessage, NsiHeaders}
import org.ogf.schemas.nsi._2013._12.connection.types.DataPlaneStateChangeRequestType
import org.ogf.schemas.nsi._2013._12.connection.types.DataPlaneStatusType

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, Boolean], timeStamp: Option[XMLGregorianCalendar]) {

  def aggregatedProvisionStatus: Boolean = childStates.values.reduce(_ && _)

  def updateState(connectionId: ConnectionId, state: Boolean, newTimeStamp: XMLGregorianCalendar) = {
    val latestTimeStamp = timeStamp.map(implicitly[Ordering[XMLGregorianCalendar]].max(_, newTimeStamp)).orElse(Some(newTimeStamp))
    copy(childStates = childStates + (connectionId -> state), timeStamp = latestTimeStamp)
  }
}

class DataPlaneStateMachine(connectionId: ConnectionId, newNotificationHeaders: () => NsiHeaders, newNotificationId: () => Int, currentVersion: () => Int, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[Boolean, DataPlaneStateMachineData, InboundMessage, OutboundMessage](false, DataPlaneStateMachineData(children, children.map(_._1 -> false), None)) {

  when(false)(PartialFunction.empty)
  when(true)(PartialFunction.empty)

  whenUnhandled {
    case Event(FromProvider(NsiRequesterMessage(headers, message: DataPlaneStateChange)), data) =>
      val newData = data.updateState(message.connectionId, message.notification.getDataPlaneStatus().isActive(), message.notification.getTimeStamp())
      goto(newData.aggregatedProvisionStatus) using newData
  }

  onTransition {
    case false -> true | true -> false =>
      Seq(ToRequester(NsiRequesterMessage(newNotificationHeaders(), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId(connectionId)
        .withNotificationId(newNotificationId())
        .withDataPlaneStatus(new DataPlaneStatusType().withVersion(currentVersion()).withActive(nextStateName).withVersionConsistent(true))
        .withTimeStamp(nextStateData.timeStamp.get)))))
  }

  def dataPlaneStatus = new DataPlaneStatusType().withVersion(currentVersion()).withActive(stateName).withVersionConsistent(true)

  def childConnectionState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
