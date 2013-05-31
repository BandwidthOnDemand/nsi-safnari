package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.ReserveType

sealed trait NsiProviderOperation extends NsiMessage {
  def optionalConnectionId: Option[ConnectionId] = None
}
sealed trait NsiQuery
sealed trait NsiCommand

case class Reserve(headers: NsiHeaders, body: ReserveType) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Option(body.getConnectionId())
}
case class ReserveCommit(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}
case class ReserveAbort(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class Provision(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}
case class Release(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class Terminate(headers: NsiHeaders, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class QuerySummary(headers: NsiHeaders, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
case class QuerySummarySync(headers: NsiHeaders, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
case class QueryRecursive(headers: NsiHeaders, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
