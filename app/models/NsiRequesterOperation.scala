package models

sealed trait NsiRequesterOperation

object NsiRequesterOperation {
  case class ReserveConfirmed() extends NsiRequesterOperation
  case class ReserveFailed() extends NsiRequesterOperation
  case class ReserveCommitConfirmed() extends NsiRequesterOperation
  case class ReserveCommitFailed() extends NsiRequesterOperation
  case class ReserveAbortConfirmed() extends NsiRequesterOperation
  case class ReserveTimeout() extends NsiRequesterOperation

  case class ProvisionConfirmed() extends NsiRequesterOperation
  case class ReleaseConfirmed() extends NsiRequesterOperation
  case class TerminateConfirmed() extends NsiRequesterOperation

  case class QuerySummaryConfirmed() extends NsiRequesterOperation
  case class QuerySummaryFailed() extends NsiRequesterOperation
  case class QueryRecursiveConfirmed() extends NsiRequesterOperation
  case class QueryRecursiveFailed() extends NsiRequesterOperation

  case class ErrorEvent() extends NsiRequesterOperation
  case class DataPlaneStateChanged() extends NsiRequesterOperation
  case class MessageDeliveryTimeout() extends NsiRequesterOperation
}
