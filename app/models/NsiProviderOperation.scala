package models

sealed trait NsiProviderOperation

object NsiProviderOperation {
  case class Reserve(correlationId: String) extends NsiProviderOperation
  case class ReserveCommit() extends NsiProviderOperation
  case class ReserveAbort() extends NsiProviderOperation

  case class Provision(correlationId: String) extends NsiProviderOperation
  case class Release(correlationId: String) extends NsiProviderOperation
  case class Terminate(correlationId: String) extends NsiProviderOperation

  case class QuerySummary(correlationId: String) extends NsiProviderOperation
  case class QuerySummarySync(correlationId: String) extends NsiProviderOperation
  case class QueryRecursive(correlationId: String) extends NsiProviderOperation
}
