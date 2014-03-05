package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

sealed trait NsiAcknowledgement {
  final def action: String = this.getClass.getSimpleName
}
case class GenericAck() extends NsiAcknowledgement
case class ReserveResponse(connectionId: String) extends NsiAcknowledgement
case class ServiceException(exception: ServiceExceptionType) extends NsiAcknowledgement
case class QuerySummarySyncConfirmed(results: Seq[QuerySummaryResultType]) extends NsiAcknowledgement
case class QueryNotificationSyncConfirmed(results: Seq[NotificationBaseType]) extends NsiAcknowledgement
case class QueryResultSyncConfirmed(results: Seq[QueryResultResponseType]) extends NsiAcknowledgement
case class ErrorAck(error: GenericErrorType) extends NsiAcknowledgement
