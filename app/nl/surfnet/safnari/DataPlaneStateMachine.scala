package nl.surfnet.safnari

import nl.surfnet.nsiv2.utils._

import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.nsiv2.messages.{ DataPlaneStateChange, NsiRequesterMessage, NsiHeaders }
import org.ogf.schemas.nsi._2013._12.connection.types.DataPlaneStateChangeRequestType
import org.ogf.schemas.nsi._2013._12.connection.types.DataPlaneStatusType

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, DataPlaneStatusType], timeStamp: Option[XMLGregorianCalendar]) {

  def highestVersion: Int = childStates.values.map(_.getVersion).max

  def isActive: Boolean = childStates.values.forall(_.isActive)

  def isVersionConsistent: Boolean = childStates.values.forall(_.isVersionConsistent) && childStates.values.forall(_.getVersion == highestVersion)

  def dataPlaneStatus = new DataPlaneStatusType()
    .withVersion(highestVersion)
    .withActive(isActive)
    .withVersionConsistent(isVersionConsistent)

  def updateState(connectionId: ConnectionId, state: DataPlaneStatusType, newTimeStamp: XMLGregorianCalendar) = {
    val latestTimeStamp = timeStamp.map(implicitly[Ordering[XMLGregorianCalendar]].max(_, newTimeStamp)).orElse(Some(newTimeStamp))
    copy(childStates = childStates + (connectionId -> state), timeStamp = latestTimeStamp)
  }
}

class DataPlaneStateMachine(connectionId: ConnectionId, newNotificationHeaders: () => NsiHeaders, newNotificationId: () => Int, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[Unit, DataPlaneStateMachineData, InboundMessage, OutboundMessage](
    (),
    DataPlaneStateMachineData(
      children,
      children.map(_._1 -> new DataPlaneStatusType()),
      None)) {

  when(()) {
    case Event(FromProvider(NsiRequesterMessage(headers, DataPlaneStateChange(notification))), data) =>
      val newData = data.updateState(notification.getConnectionId, notification.getDataPlaneStatus(), notification.getTimeStamp())
      stay using newData
  }

  onTransition {
    case _ =>
      val previous = stateData.dataPlaneStatus
      val next = nextStateData.dataPlaneStatus
      if (previous == next)
        Seq.empty
      else
        Seq(ToRequester(NsiRequesterMessage(newNotificationHeaders(), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(connectionId)
          .withNotificationId(newNotificationId())
          .withDataPlaneStatus(next)
          .withTimeStamp(nextStateData.timeStamp.get)))))
  }

  def dataPlaneStatus = nextStateData.dataPlaneStatus

  def childConnectionState(connectionId: ConnectionId) = nextStateData.childStates(connectionId)
}
