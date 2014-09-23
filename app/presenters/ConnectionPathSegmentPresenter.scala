package presenters

import nl.surfnet.safnari.ConnectionData

class ConnectionPathSegmentPresenter(private val data: ConnectionData) {
  private val statusPresenter = Nsi2StatusPresenter(data.lifecycleState, data.reservationState, data.provisionState, data.dataPlaneStatus)

  def connectionId = data.connectionId
  def providerNsa = data.providerNsa
  def providerNsaShort = shorten(data.providerNsa)
  def source = data.sourceStp
  def sourceShort = shorten(data.sourceStp)
  def destination = data.destinationStp
  def destinationShort = shorten(data.destinationStp)
  def status = statusPresenter.status
  def dataPlaneStatus = if (data.dataPlaneStatus) "active" else "inactive"
  def lastServiceException = data.lastServiceException

  private def shorten(urn: String) = urn.replace("urn:ogf:network:", "")
}

object ConnectionPathSegmentPresenter {
  def apply(data: ConnectionData) = new ConnectionPathSegmentPresenter(data)
}