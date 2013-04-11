package models

import java.net.URI
import java.util.UUID

sealed trait NsiProviderOperation {
  def headers: NsiHeaders
  def replyTo: Option[URI] = headers.replyTo
  def correlationId: UUID = headers.correlationId
}

object NsiProviderOperation {
  case class Reserve(headers: NsiHeaders) extends NsiProviderOperation
  case class ReserveCommit(headers: NsiHeaders) extends NsiProviderOperation
  case class ReserveAbort(headers: NsiHeaders) extends NsiProviderOperation

  case class Provision(headers: NsiHeaders) extends NsiProviderOperation
  case class Release(headers: NsiHeaders) extends NsiProviderOperation
  case class Terminate(headers: NsiHeaders) extends NsiProviderOperation

  case class QuerySummary(headers: NsiHeaders) extends NsiProviderOperation
  case class QuerySummarySync(headers: NsiHeaders) extends NsiProviderOperation
  case class QueryRecursive(headers: NsiHeaders) extends NsiProviderOperation
}
