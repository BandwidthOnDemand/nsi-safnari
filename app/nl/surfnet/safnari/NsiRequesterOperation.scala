package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._07.connection.types._

sealed trait NsiRequesterOperation {
  def soapActionUrl: String = {
    def deCapitalize(input: String): String = input.take(1).toLowerCase + input.drop(1)
    s"http://schemas.ogf.org/nsi/2013/07/connection/service/${deCapitalize(this.getClass().getSimpleName())}"
  }
}
sealed trait NsiNotification extends NsiRequesterOperation {
  def connectionId: ConnectionId
}

case class ReserveConfirmed(connectionId: ConnectionId, criteria: ReservationConfirmCriteriaType) extends NsiRequesterOperation

case class ReserveFailed(failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveCommitConfirmed(connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveCommitFailed(failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveAbortConfirmed(connectionId: ConnectionId) extends NsiRequesterOperation

case class ProvisionConfirmed(connectionId: ConnectionId) extends NsiRequesterOperation
case class ReleaseConfirmed(connectionId: ConnectionId) extends NsiRequesterOperation
case class TerminateConfirmed(connectionId: ConnectionId) extends NsiRequesterOperation

case class QuerySummaryConfirmed(reservations: Seq[QuerySummaryResultType]) extends NsiRequesterOperation
case class QuerySummaryFailed(failed: QueryFailedType) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(reservations: Seq[QueryRecursiveResultType]) extends NsiRequesterOperation
case class QueryRecursiveFailed(failed: QueryFailedType) extends NsiRequesterOperation

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
