package nl.surfnet.nsi

sealed trait PceMessage

case class PathComputationRequest(correlationId: CorrelationId) extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId) extends PceMessage
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[String]) extends PceMessage
