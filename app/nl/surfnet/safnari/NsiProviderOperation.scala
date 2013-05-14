package nl.surfnet.safnari

import java.net.URI
import java.util.UUID
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveType

sealed trait NsiProviderOperation extends NsiMessage {
  def optionalConnectionId: Option[ConnectionId] = None
}

case class Reserve(correlationId: CorrelationId, body: ReserveType) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Option(body.getConnectionId())
}
case class ReserveCommit(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}
case class ReserveAbort(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class Provision(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}
case class Release(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class Terminate(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class QuerySummary(correlationId: CorrelationId, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
case class QuerySummarySync(correlationId: CorrelationId, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
case class QueryRecursive(correlationId: CorrelationId, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
