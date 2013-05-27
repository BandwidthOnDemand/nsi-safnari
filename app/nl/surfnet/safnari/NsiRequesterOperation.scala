package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._04.connection.types._

sealed trait NsiRequesterOperation extends NsiMessage
sealed trait NsiNotification extends NsiRequesterOperation {
  def connectionId: ConnectionId
}

case class ReserveConfirmed(headers: NsiHeaders, connectionId: ConnectionId, criteria: ReservationConfirmCriteriaType) extends NsiRequesterOperation

case class ReserveFailed(headers: NsiHeaders, failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveCommitConfirmed(headers: NsiHeaders, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveCommitFailed(headers: NsiHeaders, failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveAbortConfirmed(headers: NsiHeaders, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveTimeout(headers: NsiHeaders, timeout: ReserveTimeoutRequestType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = timeout.getConnectionId()
}

case class ProvisionConfirmed(headers: NsiHeaders, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReleaseConfirmed(headers: NsiHeaders, connectionId: ConnectionId) extends NsiRequesterOperation
case class TerminateConfirmed(headers: NsiHeaders, connectionId: ConnectionId) extends NsiRequesterOperation

case class QuerySummaryConfirmed(headers: NsiHeaders, reservations: Seq[QuerySummaryResultType]) extends NsiRequesterOperation
case class QuerySummaryFailed(headers: NsiHeaders, failed: QueryFailedType) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(headers: NsiHeaders, reservations: Seq[QueryRecursiveResultType]) extends NsiRequesterOperation
case class QueryRecursiveFailed(headers: NsiHeaders, failed: QueryFailedType) extends NsiRequesterOperation

case class ErrorEvent(headers: NsiHeaders, error: ErrorEventType) extends NsiNotification {
  override def connectionId = error.getConnectionId()
}
case class DataPlaneStateChange(headers: NsiHeaders, connectionId: ConnectionId, status: DataPlaneStatusType, timeStamp: XMLGregorianCalendar) extends NsiNotification

case class MessageDeliveryTimeout(headers: NsiHeaders, timeStamp: XMLGregorianCalendar) extends NsiRequesterOperation
