package nl.surfnet.nsi

import java.net.URI
import java.util.UUID

sealed trait NsiProviderOperation extends NsiMessage {
  override def optionalConnectionId: Option[ConnectionId] = ???
  override def bodyDocument = ???
}

object NsiProviderOperation {
  case class Reserve(headers: NsiHeaders) extends NsiProviderOperation with Request {
    override def optionalConnectionId: Option[ConnectionId] = None
  }
  case class ReserveCommit(headers: NsiHeaders) extends NsiProviderOperation
  case class ReserveAbort(headers: NsiHeaders) extends NsiProviderOperation

  case class Provision(headers: NsiHeaders) extends NsiProviderOperation
  case class Release(headers: NsiHeaders) extends NsiProviderOperation
  case class Terminate(headers: NsiHeaders) extends NsiProviderOperation

  case class QuerySummary(headers: NsiHeaders, connectionIds: Seq[String]) extends NsiProviderOperation
  case class QuerySummarySync(headers: NsiHeaders) extends NsiProviderOperation
  case class QueryRecursive(headers: NsiHeaders) extends NsiProviderOperation
}
