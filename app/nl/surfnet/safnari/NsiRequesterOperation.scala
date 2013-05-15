package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.GenericFailedType
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._04.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._04.connection.types.DataPlaneStatusType
import javax.xml.datatype.XMLGregorianCalendar

sealed trait NsiRequesterOperation extends NsiMessage

case class ReserveConfirmed(correlationId: CorrelationId, connectionId: ConnectionId, criteria: ReservationConfirmCriteriaType) extends NsiRequesterOperation

case class ReserveFailed(correlationId: CorrelationId, failed: GenericFailedType) extends NsiRequesterOperation {
  def connectionId: ConnectionId = failed.getConnectionId()
}
case class ReserveCommitConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveCommitFailed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveAbortConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReserveTimeout(correlationId: CorrelationId) extends NsiRequesterOperation

case class ProvisionConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class ReleaseConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
case class TerminateConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation

case class QuerySummaryConfirmed(correlationId: CorrelationId, reservations: Seq[QuerySummaryResultType]) extends NsiRequesterOperation
case class QuerySummaryFailed(correlationId: CorrelationId) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(correlationId: CorrelationId) extends NsiRequesterOperation
case class QueryRecursiveFailed(correlationId: CorrelationId) extends NsiRequesterOperation

case class ErrorEvent(correlationId: CorrelationId) extends NsiRequesterOperation
case class DataPlaneStateChange(correlationId: CorrelationId, connectionId: ConnectionId, status: DataPlaneStatusType, timeStamp: XMLGregorianCalendar) extends NsiRequesterOperation
case class MessageDeliveryTimeout(correlationId: CorrelationId) extends NsiRequesterOperation
