package presenters

import nl.surfnet.nsiv2.messages._
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types.{ QuerySummaryResultType, ReservationRequestCriteriaType, ScheduleType }
import scala.collection.JavaConverters._

case class ConnectionPresenter(private val data: QuerySummaryResultType, val pendingCriteria: Option[ReservationRequestCriteriaType]) {
  private val statusPresenter = Nsi2StatusPresenter(data.getConnectionStates)

  def connectionId: ConnectionId = data.getConnectionId
  def globalReservationId = Option(data.getGlobalReservationId)
  def description = data.getDescription
  def requesterNsa = data.getRequesterNSA
  def status = statusPresenter.status
  def dataPlaneStatus = if (data.getConnectionStates.getDataPlaneStatus.isActive) "active" else "inactive"

  def committedCriteria = if (data.getCriteria.isEmpty) None else Some(data.getCriteria.asScala.maxBy(_.getVersion))

  private val schedule = committedCriteria.map(_.getSchedule).orElse(pendingCriteria.map(_.getSchedule)).getOrElse(new ScheduleType())
  private val pointToPointService = committedCriteria.flatMap(_.getPointToPointService()).orElse(pendingCriteria.flatMap(_.getPointToPointService()))

  def startTime = schedule.startTime
  def endTime = schedule.endTime
  def bandwidth = pointToPointService.map(_.getCapacity)
  def sourceStp = pointToPointService.map(_.getSourceSTP)
  def destinationStp = pointToPointService.map(_.getDestSTP)

  def committedVersion: Option[Int] = committedCriteria.map(_.getVersion)
  def pendingVersion: Option[Int] = pendingCriteria.map(_.version orElse (committedVersion.map(_ + 1)) getOrElse 1)

  def qualifier(now: DateTime) = {
    def inFuture(dt: DateTime) = dt.compareTo(now) > 0

    if (startTime.exists(inFuture)) 'future
    else if (endTime.forall(inFuture)) 'current
    else 'past
  }
}
