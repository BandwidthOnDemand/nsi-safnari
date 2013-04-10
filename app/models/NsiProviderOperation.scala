package models

import java.net.URI

case class NsiHeaders(correlationId: String, replyTo: Option[String])

sealed trait NsiProviderOperation {
  def headers: NsiHeaders
  def replyTo: Option[String] = headers.replyTo
  def correlationId: String = headers.correlationId
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
