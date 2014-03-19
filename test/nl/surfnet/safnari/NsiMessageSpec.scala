package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import java.net.URI
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import oasis.names.tc.saml._2_0.assertion.AttributeType

object NsiMessageSpec {
  val AggregatorNsa = "urn:ogf:network:nsa:aggregator-nsa"
  val RequesterNsa = "RequesterNSA"

  val SessionSecurityAttr = new SessionSecurityAttrType()
    .withAttributeOrEncryptedAttribute(new AttributeType()
    .withName("token")
    .withAttributeValue("mytoken"))

  def nsiProviderHeaders(correlationId: CorrelationId, securityAttrs: List[SessionSecurityAttrType] = Nil): NsiHeaders =
    nsiHeaders(correlationId, Some(URI.create("http://nsi-agent.example.com/")), NsiHeaders.ProviderProtocolVersion, securityAttrs)
  def nsiRequesterHeaders(correlationId: CorrelationId, securityAttrs: List[SessionSecurityAttrType] = Nil): NsiHeaders =
    nsiHeaders(correlationId, None, NsiHeaders.RequesterProtocolVersion, securityAttrs)
  def nsiHeaders(correlationId: CorrelationId, replyTo: Option[URI], protocolVersion: URI, securityAttrs: List[SessionSecurityAttrType] = Nil): NsiHeaders =
    NsiHeaders(correlationId, RequesterNsa, AggregatorNsa, replyTo, protocolVersion, securityAttrs)

  val Service = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP("networkId:A")
    .withDestSTP("networkId:B")


  val Schedule = new ScheduleType().withStartTime(DateTime.now().plusMinutes(5).toXmlGregorianCalendar).withEndTime(DateTime.now().plusMinutes(30).toXmlGregorianCalendar)
  val ConfirmCriteria = new ReservationConfirmCriteriaType().withVersion(5).withSchedule(Schedule).withServiceType("ServiceType").withPointToPointService(Service)
  val RequestCriteria = Conversion.convert(ConfirmCriteria).get

  def InitialReserveType = new ReserveType().withCriteria(RequestCriteria).withDescription("description").withGlobalReservationId("global-reservation-id")
  val InitialReserveCorrelationId = helpers.Specification.newCorrelationId

  def initialReserveMessage = NsiProviderMessage(nsiProviderHeaders(InitialReserveCorrelationId), InitialReserve(InitialReserveType, ConfirmCriteria, Service))
  val reserveConfirmed = NsiRequesterMessage(nsiRequesterHeaders(InitialReserveCorrelationId), ReserveConfirmed("ConnectionIdA", ConfirmCriteria))

  val A = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP("A").withDestSTP("X")),
    provider = ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"), NoAuthentication))
  val B = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP("X").withDestSTP("B")),
    provider = ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider"), NoAuthentication))

  object ura {
    def request(correlationId: CorrelationId, operation: NsiProviderOperation, sessionSecrutiyAttrs: List[SessionSecurityAttrType] = Nil) = {
      val headers = nsiProviderHeaders(correlationId, sessionSecrutiyAttrs)
      FromRequester(NsiProviderMessage(headers, operation))
    }
  }

  object upa {
    def acknowledge(correlationId: CorrelationId, acknowledgment: NsiAcknowledgement) =
      AckFromProvider(NsiProviderMessage(nsiProviderHeaders(correlationId), acknowledgment))
    def error(correlationId: CorrelationId, exception: ServiceExceptionType) =
      AckFromProvider(NsiProviderMessage(nsiProviderHeaders(correlationId), ServiceException(exception)))
    def response(correlationId: CorrelationId, operation: NsiRequesterOperation) =
      FromProvider(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
    def notification(correlationId: CorrelationId, operation: NsiRequesterOperation) =
      FromProvider(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
    def timeout(correlationId: CorrelationId, originalCorrelationId: CorrelationId, timestamp: DateTime) =
      MessageDeliveryFailure(correlationId, None, originalCorrelationId, URI.create("http://nsi.local/"), timestamp, "message-delivery-timeout")
  }

  object agg {
    def request(correlationId: CorrelationId, operation: NsiProviderOperation, segment: ComputedSegment = A) =
      ToProvider(NsiProviderMessage(nsiProviderHeaders(correlationId), operation), segment.provider)
    def response(correlationId: CorrelationId, operation: NsiRequesterOperation) =
      ToRequester(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
    def notification(correlationId: CorrelationId, operation: NsiNotification) =
      ToRequester(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
  }

  object pce {
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
