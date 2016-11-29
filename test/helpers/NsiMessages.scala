package helpers

import java.net.URI
import java.time.Instant
import java.time.temporal._
import javax.xml.bind.JAXBElement
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.soap._
import nl.surfnet.nsiv2.utils._
import nl.surfnet.safnari._
import oasis.names.tc.saml._2_0.assertion.AttributeType
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType

object NsiMessages {
  val AggregatorNsa = "urn:ogf:network:aggregator.tld:2015:nsa:aggregator-nsa"
  val RequesterNsa = "urn:ogf:network:requester.tld:2015:nsa:requester-nsa"

  val SessionSecurityAttr = new SessionSecurityAttrType()
    .withAttributeOrEncryptedAttribute(new AttributeType()
      .withName("token")
      .withAttributeValue("mytoken"))

  def nsiProviderHeaders(correlationId: CorrelationId, securityAttrs: List[SessionSecurityAttrType] = Nil, any: List[JAXBElement[_]] = Nil): NsiHeaders =
    nsiHeaders(correlationId, Some(URI.create("http://nsi-agent.example.com/")), NsiHeaders.ProviderProtocolVersion, securityAttrs, any)
  def nsiRequesterHeaders(correlationId: CorrelationId, securityAttrs: List[SessionSecurityAttrType] = Nil, any: List[JAXBElement[_]] = Nil): NsiHeaders =
    nsiHeaders(correlationId, None, NsiHeaders.RequesterProtocolVersion, securityAttrs, any)
  def nsiHeaders(correlationId: CorrelationId, replyTo: Option[URI], protocolVersion: URI, securityAttrs: List[SessionSecurityAttrType] = Nil, any: List[JAXBElement[_]] = Nil): NsiHeaders =
    NsiHeaders(correlationId, RequesterNsa, AggregatorNsa, replyTo, protocolVersion, securityAttrs, AnyXml(any))

  def Service = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP("networkId:A")
    .withDestSTP("networkId:B")

  val StartTime = Instant.now().plus(5, ChronoUnit.MINUTES)
  val EndTime = Instant.now().plus(30, ChronoUnit.MINUTES)

  val ConfirmedCriteriaVersion = 3
  def Schedule = new ScheduleType().withStartTime(StartTime.toXMLGregorianCalendar()).withEndTime(EndTime.toXMLGregorianCalendar())
  def ConfirmCriteria = new ReservationConfirmCriteriaType().withVersion(ConfirmedCriteriaVersion).withSchedule(Schedule).withServiceType("ServiceType").withPointToPointService(Service)
  def RequestCriteria = Conversion.convert(ConfirmCriteria).get

  def InitialReserveType = new ReserveType().withCriteria(RequestCriteria).withDescription("description").withGlobalReservationId("global-reservation-id")
  val InitialReserveCorrelationId = helpers.Specification.newCorrelationId

  def initialReserveMessage = NsiProviderMessage(nsiProviderHeaders(InitialReserveCorrelationId), InitialReserve(InitialReserveType))
  def reserveConfirmed = NsiRequesterMessage(nsiRequesterHeaders(InitialReserveCorrelationId), ReserveConfirmed("ConnectionIdA", ConfirmCriteria))

  def A = ComputedSegment(
    serviceType = ServiceType("ServiceType", Service.shallowCopy.withSourceSTP("A").withDestSTP("X")),
    provider = ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider")))
  def B = ComputedSegment(
    serviceType = ServiceType("ServiceType", Service.shallowCopy.withSourceSTP("X").withDestSTP("B")),
    provider = ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider")))

  object ura {
    def request(correlationId: CorrelationId, operation: NsiProviderOperation, sessionSecurityAttrs: List[SessionSecurityAttrType] = Nil, any: List[JAXBElement[_]] = Nil) = {
      val headers = nsiProviderHeaders(correlationId, sessionSecurityAttrs, any)
      FromRequester(NsiProviderMessage(headers, operation))
    }
  }

  object upa {
    def acknowledge(correlationId: CorrelationId, acknowledgment: NsiAcknowledgement) =
      AckFromProvider(NsiProviderMessage(nsiProviderHeaders(correlationId), acknowledgment))
    def error(correlationId: CorrelationId, exception: ServiceExceptionType) =
      AckFromProvider(NsiProviderMessage(nsiProviderHeaders(correlationId), ServiceException(exception)))
    def response(correlationId: CorrelationId, operation: NsiRequesterOperation, any: List[JAXBElement[_]] = Nil) =
      FromProvider(NsiRequesterMessage(nsiRequesterHeaders(correlationId, Nil, any), operation))
    def notification(correlationId: CorrelationId, operation: NsiRequesterOperation) =
      FromProvider(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
    def timeout(correlationId: CorrelationId, originalCorrelationId: CorrelationId, timestamp: Instant) =
      MessageDeliveryFailure(correlationId, None, originalCorrelationId, URI.create("http://nsi.local/"), timestamp, "message-delivery-timeout")
  }

  object agg {
    val ProviderReplyToUri = URI.create("http://example.com/nsi/requester")
    val PceReplyToUri = URI.create("http://example.com/pce/reply")

    def request(correlationId: CorrelationId, operation: NsiProviderOperation, segment: ComputedSegment = A, any: List[JAXBElement[_]] = Nil) = {
      val headers = NsiHeaders(correlationId, AggregatorNsa, segment.provider.nsa, Some(ProviderReplyToUri), NsiHeaders.ProviderProtocolVersion, any = AnyXml(any))
      ToProvider(NsiProviderMessage(headers, operation), segment.provider)
    }
    def response(correlationId: CorrelationId, operation: NsiRequesterOperation, any: List[JAXBElement[_]] = Nil) = {
      val headers = NsiHeaders(correlationId, RequesterNsa, AggregatorNsa, None, NsiHeaders.RequesterProtocolVersion, any = AnyXml(any))
      ToRequester(NsiRequesterMessage(headers, operation))
    }
    def notification(correlationId: CorrelationId, operation: NsiNotification) =
      ToRequester(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
  }

  object pce {
    def confirm(correlationId: CorrelationId, segments: ComputedSegment*) = {
      FromPce(PathComputationConfirmed(correlationId, segments))
    }
    def fail(correlationId: CorrelationId, error: NsiError) = {
      FromPce(PathComputationFailed(correlationId, error))
    }
    def timeout(correlationId: CorrelationId, originalCorrelationId: CorrelationId, timestamp: Instant) = {
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
