package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._04.connection.types._

sealed trait NsiRequesterOperation extends NsiMessage

case class ReserveConfirmed(correlationId: CorrelationId, connectionId: ConnectionId, criteria: ReservationConfirmCriteriaType) extends NsiRequesterOperation

case class ReserveFailed(correlationId: CorrelationId, failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveCommitConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveCommitFailed(correlationId: CorrelationId, failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveAbortConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveTimeout(correlationId: CorrelationId, timeout: ReserveTimeoutRequestType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = timeout.getConnectionId()
}

case class ProvisionConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReleaseConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class TerminateConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation

case class QuerySummaryConfirmed(correlationId: CorrelationId, reservations: Seq[QuerySummaryResultType]) extends NsiRequesterOperation
case class QuerySummaryFailed(correlationId: CorrelationId, failed: QueryFailedType) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(correlationId: CorrelationId, reservations: Seq[QueryRecursiveResultType]) extends NsiRequesterOperation
case class QueryRecursiveFailed(correlationId: CorrelationId, failed: QueryFailedType) extends NsiRequesterOperation

case class ErrorEvent(correlationId: CorrelationId, error: ErrorEventType) extends NsiRequesterOperation
case class DataPlaneStateChange(correlationId: CorrelationId, connectionId: ConnectionId, status: DataPlaneStatusType, timeStamp: XMLGregorianCalendar) extends NsiRequesterOperation
case class MessageDeliveryTimeout(correlationId: CorrelationId, timeStamp: XMLGregorianCalendar) extends NsiRequesterOperation
