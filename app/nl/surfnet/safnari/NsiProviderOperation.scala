package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

sealed trait NsiProviderOperation {
  def soapActionUrl: String = {
    s"http://schemas.ogf.org/nsi/2013/07/connection/service/${this.getClass().getSimpleName().deCapitalize}"
  }
}

sealed trait NsiProviderQuery extends NsiProviderOperation
sealed trait NsiProviderCommand extends NsiProviderOperation
sealed trait NsiProviderUpdateCommand extends NsiProviderCommand {
  def connectionId: ConnectionId
}

case class InitialReserve(body: ReserveType, criteria: ReservationConfirmCriteriaType, service: P2PServiceBaseType) extends NsiProviderCommand {
  override def soapActionUrl = "http://schemas.ogf.org/nsi/2013/07/connection/service/reserve"
}
case class ReserveCommit(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class ReserveAbort(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Provision(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class Release(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Terminate(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class QuerySummary(connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
case class QuerySummarySync(connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
case class QueryRecursive(connectionIds: Seq[ConnectionId]) extends NsiProviderQuery

case class QueryNotification(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
case class QueryNotificationSync(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery