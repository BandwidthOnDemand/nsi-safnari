package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType

sealed trait NsiProviderOperation {
  def action: String = this.getClass().getSimpleName()
  final def soapActionUrl: String = {
    s"http://schemas.ogf.org/nsi/2013/07/connection/service/${action.uncapitalize}"
  }
}

sealed trait NsiProviderQuery extends NsiProviderOperation
sealed trait NsiProviderCommand extends NsiProviderOperation {
  def optionalConnectionId: Option[ConnectionId]
}
sealed trait NsiProviderUpdateCommand extends NsiProviderCommand {
  def connectionId: ConnectionId
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class InitialReserve(body: ReserveType, criteria: ReservationConfirmCriteriaType, service: P2PServiceBaseType) extends NsiProviderCommand {
  override def action = "Reserve"
  override def optionalConnectionId: Option[ConnectionId] = None
}
case class ReserveCommit(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class ReserveAbort(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Provision(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class Release(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Terminate(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class QuerySummary(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]) extends NsiProviderQuery
case class QuerySummarySync(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]) extends NsiProviderQuery
case class QueryRecursive(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]) extends NsiProviderQuery

case class QueryNotification(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
case class QueryNotificationSync(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery