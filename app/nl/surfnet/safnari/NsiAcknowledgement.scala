package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.connection.types.QueryNotificationConfirmedType
import org.ogf.schemas.nsi._2013._07.connection.types.NotificationBaseType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

sealed trait NsiAcknowledgement
case class GenericAck() extends NsiAcknowledgement
case class ReserveResponse(connectionId: String) extends NsiAcknowledgement
case class ServiceException(exception: ServiceExceptionType) extends NsiAcknowledgement
case class QuerySummarySyncConfirmed(results: Seq[QuerySummaryResultType]) extends NsiAcknowledgement
case class QueryNotificationSyncConfirmed(results: Seq[NotificationBaseType]) extends NsiAcknowledgement
