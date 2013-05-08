package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

sealed trait NsiAcknowledgement extends NsiMessage
case class GenericAck(correlationId: CorrelationId) extends NsiAcknowledgement
case class ReserveResponse(correlationId: CorrelationId, connectionId: String) extends NsiAcknowledgement
case class ServiceException(correlationId: CorrelationId, exception: ServiceExceptionType) extends NsiAcknowledgement
case class QuerySummarySyncConfirmed(correlationId: CorrelationId, results: Seq[QuerySummaryResultType]) extends NsiAcknowledgement
