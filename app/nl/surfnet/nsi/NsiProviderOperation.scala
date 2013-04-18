package nl.surfnet.nsi

import java.net.URI
import java.util.UUID

sealed trait NsiProviderOperation extends NsiMessage {
  override def optionalConnectionId: Option[ConnectionId] = ???
  override def bodyDocument = ???
}

sealed trait NsiQuery extends NsiProviderOperation

object NsiProviderOperation {
  case class Reserve(headers: NsiHeaders) extends NsiProviderOperation {
    override def optionalConnectionId: Option[ConnectionId] = None
  }
  case class ReserveCommit(headers: NsiHeaders) extends NsiProviderOperation
  case class ReserveAbort(headers: NsiHeaders) extends NsiProviderOperation

  case class Provision(headers: NsiHeaders) extends NsiProviderOperation
  case class Release(headers: NsiHeaders) extends NsiProviderOperation
  case class Terminate(headers: NsiHeaders) extends NsiProviderOperation

  case class QuerySummary(headers: NsiHeaders, connectionIds: Seq[String]) extends NsiQuery
  case class QuerySummarySync(headers: NsiHeaders) extends NsiQuery
  case class QueryRecursive(headers: NsiHeaders) extends NsiQuery
}
