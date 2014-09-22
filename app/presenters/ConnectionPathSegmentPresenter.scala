package presenters

import nl.surfnet.safnari.ConnectionData

class ConnectionPathSegmentPresenter(private val data: ConnectionData) {
  def connectionId = data.connectionId
  def providerNsa = data.providerNsa
  def reservationState = data.reservationState.value
  def lifecycleState = data.lifecycleState.value
  def provisionState = data.provisionState.value
  def dataPlaneStatus = if (data.dataPlaneStatus) "active" else "inactive"
  def lastServiceException = data.lastServiceException
}

object ConnectionPathSegmentPresenter {
  def apply(data: ConnectionData) = new ConnectionPathSegmentPresenter(data)
}