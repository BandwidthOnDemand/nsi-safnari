package nl.surfnet.nsi

import org.ogf.schemas.nsi._2013._04.connection.types.{ ReservationConfirmCriteriaType, StpType }
import java.net.URI

sealed trait PceMessage

case class PathComputationRequest(correlationId: CorrelationId, criteria: ReservationConfirmCriteriaType) extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId) extends PceMessage
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ComputedSegment]) extends PceMessage

sealed trait ProviderAuthentication
case object NoAuthentication extends ProviderAuthentication
case class BasicAuthentication(username: String, password: String) extends ProviderAuthentication
case class OAuthAuthentication(token: String) extends ProviderAuthentication

case class ComputedSegment(sourceStp: StpType, destinationStp: StpType, providerNsa: String, providerUrl: URI, authentication: ProviderAuthentication)