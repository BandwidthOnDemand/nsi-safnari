package nl.surfnet.safnari

import java.net.URI
import java.util.UUID
import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._04.connection.types.ObjectFactory

sealed trait NsiProviderOperation extends NsiMessage {
  override def optionalConnectionId: Option[ConnectionId] = ???
  override def asDocument: Document = ???
}

case class Reserve(correlationId: CorrelationId, body: ReserveType) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Option(body.getConnectionId())

  override def asDocument = NsiMessage.marshal(new ObjectFactory().createReserve(body))
}

case class ReserveCommit(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class ReserveAbort(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation with NsiCommand {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class Provision(correlationId: CorrelationId) extends NsiProviderOperation with NsiCommand
case class Release(correlationId: CorrelationId) extends NsiProviderOperation with NsiCommand
case class Terminate(correlationId: CorrelationId) extends NsiProviderOperation with NsiCommand

case class QuerySummary(correlationId: CorrelationId, connectionIds: Seq[ConnectionId]) extends NsiProviderOperation with NsiQuery
case class QuerySummarySync(correlationId: CorrelationId) extends NsiProviderOperation with NsiQuery
case class QueryRecursive(correlationId: CorrelationId) extends NsiProviderOperation with NsiQuery
