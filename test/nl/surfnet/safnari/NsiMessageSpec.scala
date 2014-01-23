package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import java.net.URI
import org.joda.time.DateTime

object NsiMessageSpec {
  val AggregatorNsa = "urn:ogf:network:nsa:aggregator-nsa"

  def headers(correlationId: CorrelationId, protocolVersion: URI) = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, None, protocolVersion)
  val Service = new P2PServiceBaseType().
    withDirectionality(DirectionalityType.BIDIRECTIONAL).
    withCapacity(100).
    withSourceSTP("networkId:A").
    withDestSTP("networkId:B")

  val Schedule = new ScheduleType().withStartTime(DateTime.now().plusMinutes(5).toXmlGregorianCalendar).withEndTime(DateTime.now().plusMinutes(30).toXmlGregorianCalendar)
  val ConfirmCriteria = new ReservationConfirmCriteriaType().withVersion(5).withSchedule(Schedule).withServiceType("ServiceType").withPointToPointService(Service)
  val RequestCriteria = Conversion.convert(ConfirmCriteria).get

  def InitialReserveType = new ReserveType().withCriteria(RequestCriteria).withDescription("description").withGlobalReservationId("global-reservation-id")
  val InitialReserveCorrelationId = helpers.Specification.newCorrelationId

  def initialReserveMessage = NsiProviderMessage(headers(InitialReserveCorrelationId, NsiHeaders.ProviderProtocolVersion), InitialReserve(InitialReserveType, ConfirmCriteria, Service))
  val reserveConfirmed = NsiRequesterMessage(headers(InitialReserveCorrelationId, NsiHeaders.RequesterProtocolVersion), ReserveConfirmed("ConnectionIdA", ConfirmCriteria))

  val A = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP("A").withDestSTP("X")),
    provider = ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"), NoAuthentication))
  val B = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP("X").withDestSTP("B")),
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
    def timeout(correlationId: CorrelationId, originalCorrelationId: CorrelationId, timestamp: DateTime) = {
      MessageDeliveryFailure(correlationId, None, originalCorrelationId, URI.create("http://nsi.local/"), timestamp, "message-delivery-timeout")
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
    def timeout(correlationId: CorrelationId, originalCorrelationId: CorrelationId, timestamp: DateTime) = {
      MessageDeliveryFailure(correlationId, None, originalCorrelationId, URI.create("http://pce.local/"), timestamp, "message-delivery-timeout")
    }
    def failedAck(correlationId: CorrelationId, status: Int = 400, statusText: String = "Bad Request", message: String = "Find path request not accepted") = {
      AckFromPce(PceFailed(correlationId, status, statusText, message))
    }
    def acceptedAck(correlationId: CorrelationId) = {
      AckFromPce(PceAccepted(correlationId))
    }
  }

}
