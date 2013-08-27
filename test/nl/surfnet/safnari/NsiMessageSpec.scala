package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types._
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.types.DirectionalityType
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import java.net.URI

object NsiMessageSpec {
  def headers(correlationId: CorrelationId, protocolVersion: URI) = NsiHeaders(correlationId, "RequesterNSA", "ProviderNSA", None, protocolVersion)
  val Service = new P2PServiceBaseType().
    withDirectionality(DirectionalityType.BIDIRECTIONAL).
    withSourceSTP(new StpType().withNetworkId("networkId").withLocalId("source-localId")).
    withDestSTP(new StpType().withNetworkId("networkId").withLocalId("dest-localId"))
  val Criteria = new ReservationRequestCriteriaType().
    withSchedule(new ScheduleType()).
    withPointToPointService(Service)

  val ConfirmCriteria = Conversion.invert(Criteria).right.get

  def InitialReserveType = new ReserveType().withCriteria(Criteria)
  val CorrelationId = newCorrelationId

  def initialReserveMessage = InitialReserve(headers(CorrelationId, NsiHeaders.ProviderProtocolVersion), InitialReserveType, ConfirmCriteria, Service)

  val reserveConfirmed = ReserveConfirmed(headers(CorrelationId, NsiHeaders.RequesterProtocolVersion), "ConnectionIdA", ConfirmCriteria)
}
