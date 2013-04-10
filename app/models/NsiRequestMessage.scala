package models

sealed trait NsiRequestMessage

object NsiRequestMessage {
  case class Reserve(correlationId: String) extends NsiRequestMessage
  case class ReserveCommit() extends NsiRequestMessage
  case class ReserveAbort() extends NsiRequestMessage

  case class Provision(correlationId: String) extends NsiRequestMessage
  case class Release(correlationId: String) extends NsiRequestMessage
  case class Terminate(correlationId: String) extends NsiRequestMessage

  case class QuerySummary(correlationId: String) extends NsiRequestMessage
  case class QuerySummarySync(correlationId: String) extends NsiRequestMessage
  case class QueryRecursive(correlationId: String) extends NsiRequestMessage
}