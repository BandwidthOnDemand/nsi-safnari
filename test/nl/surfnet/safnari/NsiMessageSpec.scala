package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types._

object NsiMessageSpec {
  def withEnvelope[T <: NsiMessage](message: T) = NsiEnvelope(NsiHeaders(message.correlationId, "RequesterNSA", "ProviderNSA", None), message)
  val Criteria = new ReservationRequestCriteriaType().
    withSchedule(new ScheduleType()).
    withBandwidth(100).
    withServiceAttributes(new TypeValuePairListType()).
    withPath(new PathType().withDirectionality(DirectionalityType.BIDIRECTIONAL).withSourceSTP(new StpType().withNetworkId("networkId").withLocalId("source-localId")).withDestSTP(new StpType().withNetworkId("networkId").withLocalId("dest-localId")))

  def InitialReserveType = new ReserveType().withCriteria(Criteria)
  val CorrelationId = newCorrelationId

  def initialReserveMessage = withEnvelope(Reserve(CorrelationId, InitialReserveType))
}
