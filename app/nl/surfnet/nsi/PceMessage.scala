package nl.surfnet.nsi

import org.ogf.schemas.nsi._2013._04.connection.types.ReservationRequestCriteriaType

sealed trait PceMessage

case class PathComputationRequest(correlationId: CorrelationId, criteria: ReservationRequestCriteriaType) extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId) extends PceMessage
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ReservationRequestCriteriaType]) extends PceMessage
