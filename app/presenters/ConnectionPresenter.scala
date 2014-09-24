package presenters

import nl.surfnet.safnari.ConnectionId
import org.ogf.schemas.nsi._2013._12.connection.types.QuerySummaryResultType

case class ConnectionPresenter(data: QuerySummaryResultType) {
  private val statusPresenter = Nsi2StatusPresenter(data.getConnectionStates)

  def connectionId: ConnectionId = data.getConnectionId
  def globalReservationId = data.getGlobalReservationId
  def description = data.getDescription
  def requesterNsa = data.getRequesterNSA
  def status = statusPresenter.status
  def dataPlaneStatus = if (data.getConnectionStates.getDataPlaneStatus.isActive) "active" else "inactive"
}
