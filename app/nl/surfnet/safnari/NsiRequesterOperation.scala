package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types._

sealed trait NsiRequesterOperation {
  final def action = this.getClass().getSimpleName()
  final def soapActionUrl: String = s"http://schemas.ogf.org/nsi/2013/12/connection/service/${action.uncapitalize}"
}
sealed trait NsiNotification extends NsiRequesterOperation {
  def connectionId: ConnectionId
  def notification: NotificationBaseType
}
sealed trait NsiCommandReply extends NsiRequesterOperation

case class ReserveConfirmed(connectionId: ConnectionId, criteria: ReservationConfirmCriteriaType) extends NsiCommandReply
case class ReserveFailed(failed: GenericFailedType) extends NsiCommandReply {
  def connectionId: ConnectionId = failed.getConnectionId()
}

case class ReserveCommitConfirmed(connectionId: ConnectionId) extends NsiCommandReply
case class ReserveCommitFailed(failed: GenericFailedType) extends NsiCommandReply {
  def connectionId: ConnectionId = failed.getConnectionId()
}

case class ReserveAbortConfirmed(connectionId: ConnectionId) extends NsiCommandReply

case class ProvisionConfirmed(connectionId: ConnectionId) extends NsiCommandReply
case class ReleaseConfirmed(connectionId: ConnectionId) extends NsiCommandReply
case class TerminateConfirmed(connectionId: ConnectionId) extends NsiCommandReply

case class QuerySummaryConfirmed(reservations: Seq[QuerySummaryResultType]) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(reservations: Seq[QueryRecursiveResultType]) extends NsiRequesterOperation
case class QueryNotificationConfirmed(notifications: Seq[NotificationBaseType]) extends NsiRequesterOperation
case class QueryResultConfirmed(results: Seq[QueryResultResponseType]) extends NsiRequesterOperation

case class Error(error: GenericErrorType) extends NsiRequesterOperation

case class ErrorEvent(override val notification: ErrorEventType) extends NsiNotification {
  override def connectionId = notification.getConnectionId()
}
case class DataPlaneStateChange(override val notification: DataPlaneStateChangeRequestType) extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}
case class ReserveTimeout(override val notification: ReserveTimeoutRequestType) extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}

case class MessageDeliveryTimeout(override val notification: MessageDeliveryTimeoutRequestType) extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}
