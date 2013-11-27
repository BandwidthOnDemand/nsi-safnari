package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._07.connection.types._

sealed trait NsiRequesterOperation {
  def soapActionUrl: String = {
    s"http://schemas.ogf.org/nsi/2013/07/connection/service/${this.getClass().getSimpleName().deCapitalize}"
  }
}
sealed trait NsiNotification extends NsiRequesterOperation {
  def connectionId: ConnectionId
}
sealed trait NsiQueryRecursiveResponse extends NsiRequesterOperation
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
case class QuerySummaryFailed(failed: QueryFailedType) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(reservations: Seq[QueryRecursiveResultType]) extends NsiQueryRecursiveResponse
case class QueryRecursiveFailed(failed: QueryFailedType) extends NsiQueryRecursiveResponse
case class QueryNotificationConfirmed(notifications: Seq[NotificationBaseType]) extends NsiRequesterOperation

case class ErrorEvent(error: ErrorEventType) extends NsiNotification {
  override def connectionId = error.getConnectionId()
}
case class DataPlaneStateChange(notification: DataPlaneStateChangeRequestType) extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}
case class ReserveTimeout(timeout: ReserveTimeoutRequestType) extends NsiNotification {
  def connectionId: ConnectionId = timeout.getConnectionId()
}

case class MessageDeliveryTimeout(timeout: MessageDeliveryTimeoutRequestType) extends NsiNotification {
  def connectionId: ConnectionId = timeout.getConnectionId()
}
