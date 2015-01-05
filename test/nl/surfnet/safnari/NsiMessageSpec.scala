package nl.surfnet.safnari

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.soap._
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import java.net.URI
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import oasis.names.tc.saml._2_0.assertion.AttributeType
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType

object NsiMessageSpec {
  val AggregatorNsa = "urn:ogf:network:nsa:aggregator-nsa"
  val RequesterNsa = "RequesterNSA"

  val SessionSecurityAttr = new SessionSecurityAttrType()
    .withAttributeOrEncryptedAttribute(new AttributeType()
      .withName("token")
      .withAttributeValue("mytoken"))

  def nsiProviderHeaders(correlationId: CorrelationId, securityAttrs: List[SessionSecurityAttrType] = Nil, connectionTrace: List[ConnectionType] = Nil): NsiHeaders =
    nsiHeaders(correlationId, Some(URI.create("http://nsi-agent.example.com/")), NsiHeaders.ProviderProtocolVersion, securityAttrs, connectionTrace)
  def nsiRequesterHeaders(correlationId: CorrelationId, securityAttrs: List[SessionSecurityAttrType] = Nil): NsiHeaders =
    nsiHeaders(correlationId, None, NsiHeaders.RequesterProtocolVersion, securityAttrs)
  def nsiHeaders(correlationId: CorrelationId, replyTo: Option[URI], protocolVersion: URI, securityAttrs: List[SessionSecurityAttrType] = Nil, connectionTrace: List[ConnectionType] = Nil): NsiHeaders =
    NsiHeaders(correlationId, RequesterNsa, AggregatorNsa, replyTo, protocolVersion, securityAttrs, connectionTrace)

  val Service = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP("networkId:A")
    .withDestSTP("networkId:B")

  val StartTime = DateTime.now().plusMinutes(5)
  val EndTime = DateTime.now().plusMinutes(30)

  val ConfirmedCriteriaVersion = 3
  def Schedule = new ScheduleType().withStartTime(StartTime.toXmlGregorianCalendar).withEndTime(EndTime.toXmlGregorianCalendar)
  def ConfirmCriteria = new ReservationConfirmCriteriaType().withVersion(ConfirmedCriteriaVersion).withSchedule(Schedule).withServiceType("ServiceType").withPointToPointService(Service)
  def RequestCriteria = Conversion.convert(ConfirmCriteria).get

  def InitialReserveType = new ReserveType().withCriteria(RequestCriteria).withDescription("description").withGlobalReservationId("global-reservation-id")
  val InitialReserveCorrelationId = helpers.Specification.newCorrelationId

  def initialReserveMessage = NsiProviderMessage(nsiProviderHeaders(InitialReserveCorrelationId), InitialReserve(InitialReserveType))
  def reserveConfirmed = NsiRequesterMessage(nsiRequesterHeaders(InitialReserveCorrelationId), ReserveConfirmed("ConnectionIdA", ConfirmCriteria))

  val A = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP("A").withDestSTP("X")),
    provider = ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider")))
  val B = ComputedSegment(
    serviceType = ServiceType("ServiceType", new P2PServiceBaseType().withCapacity(100).withSourceSTP("X").withDestSTP("B")),
    provider = ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider")))

  object ura {
    def request(correlationId: CorrelationId, operation: NsiProviderOperation, sessionSecurityAttrs: List[SessionSecurityAttrType] = Nil, connectionTrace: List[ConnectionType] = Nil) = {
      val headers = nsiProviderHeaders(correlationId, sessionSecurityAttrs, connectionTrace)
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
    def fail(correlationId: CorrelationId, error: NsiError) = {
      FromPce(PathComputationFailed(correlationId, error))
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
