package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types._

object NsiMessageSpec {
  def headers(correlationId: CorrelationId) = NsiHeaders(correlationId, "RequesterNSA", "ProviderNSA", None)
  val Criteria = new ReservationRequestCriteriaType().
    withSchedule(new ScheduleType()).
    withBandwidth(100).
    withServiceAttributes(new TypeValuePairListType()).
    withPath(new PathType().withDirectionality(DirectionalityType.BIDIRECTIONAL).withSourceSTP(new StpType().withNetworkId("networkId").withLocalId("source-localId")).withDestSTP(new StpType().withNetworkId("networkId").withLocalId("dest-localId")))
  val ConfirmCriteria = Conversion.invert(Criteria).right.get

  def InitialReserveType = new ReserveType().withCriteria(Criteria)
  val CorrelationId = newCorrelationId

  def initialReserveMessage = Reserve(headers(CorrelationId), InitialReserveType)

  val reserveConfirmed = ReserveConfirmed(headers(CorrelationId), "ConnectionIdA", ConfirmCriteria)
}
