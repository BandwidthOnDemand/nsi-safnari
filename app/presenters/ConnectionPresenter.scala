package presenters

import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types.{QuerySummaryResultType, ReservationConfirmCriteriaType}

case class ConnectionPresenter(private val data: QuerySummaryResultType,
                               private val criteria: ReservationConfirmCriteriaType) {
  private val statusPresenter = Nsi2StatusPresenter(data.getConnectionStates)

  def connectionId: ConnectionId = data.getConnectionId
  def globalReservationId = Option(data.getGlobalReservationId)
  def description = data.getDescription
  def requesterNsa = data.getRequesterNSA
  def status = statusPresenter.status
  def dataPlaneStatus = if (data.getConnectionStates.getDataPlaneStatus.isActive) "active" else "inactive"
  def startTime = Option(criteria.getSchedule.getStartTime)
  def endTime = Option(criteria.getSchedule.getEndTime)
  def bandwidth = criteria.getPointToPointService().map(_.getCapacity)
  def version = criteria.getVersion
  def sourceStp = criteria.getPointToPointService().map(_.getSourceSTP)
  def destinationStp = criteria.getPointToPointService().map(_.getDestSTP)

  def qualifier(now: DateTime) = {
    def inFuture(dt: DateTime) = dt.compareTo(now) > 0

    if (startTime.map(_.toDateTime).exists(inFuture)) 'future
    else if (endTime.map(_.toDateTime).forall(inFuture)) 'active
    else 'past
  }
}
