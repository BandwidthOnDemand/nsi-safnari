package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

sealed trait NsiProviderOperation extends NsiMessage

sealed trait NsiProviderQuery extends NsiProviderOperation
sealed trait NsiProviderCommand extends NsiProviderOperation
sealed trait NsiProviderUpdateCommand extends NsiProviderCommand {
  def connectionId: ConnectionId
}

case class InitialReserve(headers: NsiHeaders, body: ReserveType, criteria: ReservationConfirmCriteriaType, service: P2PServiceBaseType) extends NsiProviderCommand
case class ReserveCommit(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class ReserveAbort(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Provision(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class Release(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Terminate(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class QuerySummary(headers: NsiHeaders, connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
case class QuerySummarySync(headers: NsiHeaders, connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
case class QueryRecursive(headers: NsiHeaders, connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
