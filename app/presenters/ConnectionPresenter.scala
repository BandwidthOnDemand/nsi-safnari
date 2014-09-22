package presenters

import nl.surfnet.safnari.ConnectionId
import org.ogf.schemas.nsi._2013._12.connection.types.QuerySummaryResultType

class ConnectionPresenter(val data: QuerySummaryResultType) {
  def connectionId: ConnectionId = data.getConnectionId
  def globalReservationId = data.getGlobalReservationId
  def description = data.getDescription
  def requesterNsa = data.getRequesterNSA
  def reservationState = data.getConnectionStates.getReservationState.value
  def provisionState = data.getConnectionStates.getProvisionState.value
  def lifecycleState = data.getConnectionStates.getLifecycleState.value
  def dataPlaneStatus = if (data.getConnectionStates.getDataPlaneStatus.isActive) "active" else "inactive"
}

object ConnectionPresenter {
  def apply(data: QuerySummaryResultType) = new ConnectionPresenter(data)
}