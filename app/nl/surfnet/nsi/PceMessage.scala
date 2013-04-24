package nl.surfnet.nsi

import org.ogf.schemas.nsi._2013._04.connection.types.ReservationConfirmCriteriaType

sealed trait PceMessage

case class PathComputationRequest(correlationId: CorrelationId, criteria: ReservationConfirmCriteriaType) extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId) extends PceMessage
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ReservationConfirmCriteriaType]) extends PceMessage
