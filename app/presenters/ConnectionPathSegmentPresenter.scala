package presenters

import nl.surfnet.safnari.ConnectionData

class ConnectionPathSegmentPresenter(private val data: ConnectionData) {
  private val statusPresenter = Nsi2StatusPresenter(data.lifecycleState, data.reservationState, data.provisionState, data.dataPlaneStatus)

  def connectionId = data.connectionId
  def providerNsa = data.providerNsa
  def source = data.sourceStp
  def destination = data.destinationStp
  def status = statusPresenter.status
  def dataPlaneStatus = if (data.dataPlaneStatus) "active" else "inactive"
  def lastServiceException = data.lastServiceException
}

object ConnectionPathSegmentPresenter {
  def apply(data: ConnectionData) = new ConnectionPathSegmentPresenter(data)
}