package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._07.connection.types._

sealed trait NsiRequesterOperation
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
case class DataPlaneStateChange(connectionId: ConnectionId, status: DataPlaneStatusType, timeStamp: XMLGregorianCalendar) extends NsiNotification
case class ReserveTimeout(timeout: ReserveTimeoutRequestType) extends NsiNotification {
  def connectionId: ConnectionId = timeout.getConnectionId()
}

case class MessageDeliveryTimeout(correlationId: CorrelationId, timeStamp: XMLGregorianCalendar) extends NsiRequesterOperation
