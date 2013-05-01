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

sealed trait NsiQuery extends NsiProviderOperation

case class Reserve(correlationId: CorrelationId, body: ReserveType) extends NsiProviderOperation {
  override def optionalConnectionId: Option[ConnectionId] = Option(body.getConnectionId())

  override def asDocument = NsiMessage.marshal(new ObjectFactory().createReserve(body))
}

case class ReserveCommit(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class ReserveAbort(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiProviderOperation {
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

case class Provision(correlationId: CorrelationId) extends NsiProviderOperation
case class Release(correlationId: CorrelationId) extends NsiProviderOperation
case class Terminate(correlationId: CorrelationId) extends NsiProviderOperation

case class QuerySummary(correlationId: CorrelationId, connectionIds: Seq[ConnectionId]) extends NsiQuery
case class QuerySummarySync(correlationId: CorrelationId) extends NsiQuery
case class QueryRecursive(correlationId: CorrelationId) extends NsiQuery
