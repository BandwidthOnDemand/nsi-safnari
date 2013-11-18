package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types._
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.types.DirectionalityType
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import java.net.URI
import org.joda.time.DateTime

object NsiMessageSpec {
  val AggregatorNsa = "urn:ogf:network:nsa:surfnet-nsi-safnari"

  def headers(correlationId: CorrelationId, protocolVersion: URI) = NsiHeaders(correlationId, "RequesterNSA", "ProviderNSA", None, protocolVersion)
  val Service = new P2PServiceBaseType().
    withDirectionality(DirectionalityType.BIDIRECTIONAL).
    withCapacity(100).
    withSourceSTP(new StpType().withNetworkId("networkId").withLocalId("A")).
    withDestSTP(new StpType().withNetworkId("networkId").withLocalId("B"))

  val Schedule = new ScheduleType()
  val ConfirmCriteria = new ReservationConfirmCriteriaType().withSchedule(Schedule).withServiceType("ServiceType").withPointToPointService(Service)
  val RequestCriteria = Conversion.convert(ConfirmCriteria).right.get

  def InitialReserveType = new ReserveType().withCriteria(RequestCriteria)
  val InitialReserveCorrelationId = newCorrelationId

  def initialReserveMessage = NsiProviderMessage(headers(InitialReserveCorrelationId, NsiHeaders.ProviderProtocolVersion), InitialReserve(InitialReserveType, ConfirmCriteria, Service))
  val reserveConfirmed = NsiRequesterMessage(headers(InitialReserveCorrelationId, NsiHeaders.RequesterProtocolVersion), ReserveConfirmed("ConnectionIdA", ConfirmCriteria))

  val A = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP(new StpType().withLocalId("A")).withDestSTP(new StpType().withLocalId("X"))),
    provider = ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"), NoAuthentication))
  val B = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP(new StpType().withLocalId("X")).withDestSTP(new StpType().withLocalId("B"))),
    provider = ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider"), NoAuthentication))

  object ura {
    def request(correlationId: CorrelationId, operation: NsiProviderOperation) = {
      val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, Some(URI.create("http://ultimate.requester.example.com/")), NsiHeaders.ProviderProtocolVersion)
      FromRequester(NsiProviderMessage(headers, operation))
    }
  }
  object upa {
    def acknowledge(correlationId: CorrelationId, acknowledgment: NsiAcknowledgement) = {
      val headers = NsiHeaders(correlationId, AggregatorNsa, "ProviderNSA", None, NsiHeaders.ProviderProtocolVersion)
      AckFromProvider(NsiProviderMessage(headers, acknowledgment))
    }
    def error(correlationId: CorrelationId, exception: ServiceExceptionType) = {
      val headers = NsiHeaders(correlationId, AggregatorNsa, "ProviderNSA", None, NsiHeaders.ProviderProtocolVersion)
      AckFromProvider(NsiProviderMessage(headers, ServiceException(exception)))
    }
    def response(correlationId: CorrelationId, operation: NsiRequesterOperation) = {
      val headers = NsiHeaders(correlationId, AggregatorNsa, "ProviderNSA", None, NsiHeaders.RequesterProtocolVersion)
      FromProvider(NsiRequesterMessage(headers, operation))
    }
    def notification(correlationId: CorrelationId, operation: NsiRequesterOperation) = {
      val headers = NsiHeaders(correlationId, AggregatorNsa, "ProviderNSA", None, NsiHeaders.RequesterProtocolVersion)
      FromProvider(NsiRequesterMessage(headers, operation))
    }
    def timeout(correlationId: CorrelationId, timestamp: DateTime) = {
      MessageDeliveryFailure(correlationId, None, URI.create("http://nsi.local/"), timestamp, "message-delivery-timeout")
    }
  }
  object agg {
    def request(correlationId: CorrelationId, operation: NsiProviderOperation, segment: ComputedSegment = A) = {
      val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
      ToProvider(NsiProviderMessage(headers, operation), segment.provider)
    }
    def response(correlationId: CorrelationId, operation: NsiRequesterOperation) = {
      val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
      ToRequester(NsiRequesterMessage(headers, operation))
    }
    def notification(correlationId: CorrelationId, operation: NsiNotification) = {
      val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
      ToRequester(NsiRequesterMessage(headers, operation))
    }
  }
  object pce {
    // PCE operations
    def confirm(correlationId: CorrelationId, segments: ComputedSegment*) = {
      FromPce(PathComputationConfirmed(correlationId, segments))
    }
    def fail(correlationId: CorrelationId, message: String = "error-message") = {
      FromPce(PathComputationFailed(correlationId, message))
    }
    def timeout(correlationId: CorrelationId, timestamp: DateTime) = {
      MessageDeliveryFailure(correlationId, None, URI.create("http://pce.local/"), timestamp, "message-delivery-timeout")
    }
  }

}
