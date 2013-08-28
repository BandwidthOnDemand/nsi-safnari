package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

sealed trait NsiProviderOperation

sealed trait NsiProviderQuery extends NsiProviderOperation
sealed trait NsiProviderCommand extends NsiProviderOperation
sealed trait NsiProviderUpdateCommand extends NsiProviderCommand {
  def connectionId: ConnectionId
}

case class InitialReserve(body: ReserveType, criteria: ReservationConfirmCriteriaType, service: P2PServiceBaseType) extends NsiProviderCommand
case class ReserveCommit(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class ReserveAbort(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Provision(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class Release(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Terminate(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class QuerySummary(connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
case class QuerySummarySync(connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
case class QueryRecursive(connectionIds: Seq[ConnectionId]) extends NsiProviderQuery
