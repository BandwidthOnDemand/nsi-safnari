package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

sealed trait NsiAcknowledgement extends NsiMessage
case class GenericAck(headers: NsiHeaders) extends NsiAcknowledgement
case class ReserveResponse(headers: NsiHeaders, connectionId: String) extends NsiAcknowledgement
case class ServiceException(headers: NsiHeaders, exception: ServiceExceptionType) extends NsiAcknowledgement
case class QuerySummarySyncConfirmed(headers: NsiHeaders, results: Seq[QuerySummaryResultType]) extends NsiAcknowledgement
