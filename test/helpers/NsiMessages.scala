package helpers

import java.net.URI
import java.time.Instant
import java.time.temporal._
import jakarta.xml.bind.JAXBElement
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._
import nl.surfnet.safnari._
import oasis.names.tc.saml._2_0.assertion.AttributeType
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import org.ogf.schemas.nsi._2015._04.connection.pathtrace.{
  PathTraceType,
  PathType,
  ObjectFactory => PathTraceTypeOF,
  SegmentType,
  StpType
}
import scala.jdk.CollectionConverters._

object NsiMessages {
  val AggregatorNsa = "urn:ogf:network:aggregator.tld:2015:nsa:aggregator-nsa"
  val RequesterNsa = "urn:ogf:network:requester.tld:2015:nsa:requester-nsa"

  val Aggregator =
    ProviderEndPoint(AggregatorNsa, URI.create("http://nsi-agent.example.com/provider"))

  val SessionSecurityAttr = new SessionSecurityAttrType()
    .withAttributeOrEncryptedAttribute(
      new AttributeType()
        .withName("token")
        .withAttributeValue("mytoken")
    )

  def nsiProviderHeaders(
      provider: ProviderEndPoint,
      correlationId: CorrelationId,
      securityAttrs: List[SessionSecurityAttrType] = Nil,
      any: List[JAXBElement[_]] = Nil
  ): NsiHeaders =
    NsiHeaders(
      correlationId,
      AggregatorNsa,
      provider.nsa,
      Some(URI.create("http://nsi-agent.example.com/")),
      NsiHeaders.ProviderProtocolVersion,
      securityAttrs,
      XmlAny(any)
    )
  def nsiRequesterHeaders(
      correlationId: CorrelationId,
      securityAttrs: List[SessionSecurityAttrType] = Nil,
      any: List[JAXBElement[_]] = Nil
  ): NsiHeaders =
    nsiHeaders(correlationId, None, NsiHeaders.RequesterProtocolVersion, securityAttrs, any)
  def nsiHeaders(
      correlationId: CorrelationId,
      replyTo: Option[URI],
      protocolVersion: URI,
      securityAttrs: List[SessionSecurityAttrType] = Nil,
      any: List[JAXBElement[_]] = Nil
  ): NsiHeaders =
    NsiHeaders(
      correlationId,
      RequesterNsa,
      AggregatorNsa,
      replyTo,
      protocolVersion,
      securityAttrs,
      XmlAny(any)
    )

  def Service = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP("networkId:A")
    .withDestSTP("networkId:B")

  val StartTime = Instant.now().plus(5, ChronoUnit.MINUTES)
  val EndTime = Instant.now().plus(30, ChronoUnit.MINUTES)

  val ConfirmedCriteriaVersion = 3
  def Schedule = new ScheduleType()
    .withStartTime(StartTime.toXMLGregorianCalendar())
    .withEndTime(EndTime.toXMLGregorianCalendar())
  def ConfirmCriteria = new ReservationConfirmCriteriaType()
    .withVersion(ConfirmedCriteriaVersion)
    .withSchedule(Schedule)
    .withServiceType("ServiceType")
    .withPointToPointService(Service)
  def RequestCriteria = new ReservationRequestCriteriaType()
    .withVersion(ConfirmedCriteriaVersion)
    .withSchedule(Schedule)
    .withServiceType("ServiceType")
    .withPointToPointService(Service)

  def InitialReserveType = new ReserveType()
    .withCriteria(RequestCriteria)
    .withDescription("description")
    .withGlobalReservationId("global-reservation-id")
  val InitialReserveCorrelationId = helpers.Specification.newCorrelationId()

  def initialReserveMessage = NsiProviderMessage(
    nsiProviderHeaders(Aggregator, InitialReserveCorrelationId).copy(requesterNSA = RequesterNsa),
    InitialReserve(InitialReserveType)
  )
  def reserveConfirmed = NsiRequesterMessage(
    nsiRequesterHeaders(InitialReserveCorrelationId),
    ReserveConfirmed("ConnectionIdA", ConfirmCriteria)
  )

  def A = ComputedSegment(
    serviceType =
      ServiceType("ServiceType", Service.shallowCopy.withSourceSTP("A").withDestSTP("X")),
    provider = ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"))
  )
  def B = ComputedSegment(
    serviceType =
      ServiceType("ServiceType", Service.shallowCopy.withSourceSTP("X").withDestSTP("B")),
    provider =
      ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider"))
  )

  def emptyPathTrace(nsa: String, connectionId: ConnectionId) = pathTrace(nsa, connectionId)

  def pathTrace(
      nsa: String,
      connectionId: ConnectionId,
      segments: ((String, ConnectionId), List[String])*
  ) = {
    val result = new PathTraceType().withId(nsa).withConnectionId(connectionId)
    if (segments.nonEmpty) {
      result.getPath.add(
        new PathType().withSegment((for {
          (((nsa, connectionId), stps), order) <- segments.zipWithIndex
        } yield {
          new SegmentType()
            .withId(nsa)
            .withConnectionId(connectionId)
            .withOrder(order)
            .withStp((for {
              (stp, index) <- stps.zipWithIndex
            } yield new StpType().withOrder(index).withValue(stp)).asJava)
        }).asJava)
      )
    }
    new PathTraceTypeOF().createPathTrace(result)
  }

  object ura {
    def request(
        correlationId: CorrelationId,
        operation: NsiProviderOperation,
        sessionSecurityAttrs: List[SessionSecurityAttrType] = Nil,
        any: List[JAXBElement[_]] = Nil
    ) = {
      val headers = nsiProviderHeaders(Aggregator, correlationId, sessionSecurityAttrs, any)
        .copy(requesterNSA = RequesterNsa)
      FromRequester(NsiProviderMessage(headers, operation))
    }
  }

  implicit class ProviderEndPointOps(provider: ProviderEndPoint) {
    def acknowledge(correlationId: CorrelationId, acknowledgment: NsiAcknowledgement) =
      AckFromProvider(
        NsiProviderMessage(nsiProviderHeaders(provider, correlationId).forSyncAck, acknowledgment)
      )
    def error(correlationId: CorrelationId, exception: ServiceExceptionType) =
      AckFromProvider(
        NsiProviderMessage(
          nsiProviderHeaders(provider, correlationId).forSyncAck,
          ServiceException(exception)
        )
      )
    def response(
        correlationId: CorrelationId,
        operation: NsiRequesterOperation,
        any: List[JAXBElement[_]] = Nil
    ) =
      FromProvider(
        NsiRequesterMessage(
          nsiProviderHeaders(provider, correlationId, Nil, Nil).forAsyncReply
            .copy(any = XmlAny(any)),
          operation
        )
      )
    def notification(correlationId: CorrelationId, operation: NsiRequesterOperation) =
      FromProvider(NsiRequesterMessage(nsiRequesterHeaders(correlationId), operation))
    def timeout(
        correlationId: CorrelationId,
        originalCorrelationId: CorrelationId,
        timestamp: Instant
    ) =
      MessageDeliveryFailure(
        correlationId,
        None,
        originalCorrelationId,
        URI.create("http://nsi.local/"),
        timestamp,
        "message-delivery-timeout"
      )
  }

  val upa = ProviderEndPointOps(A.provider)

  object agg {
    val ProviderReplyToUri = URI.create("http://example.com/nsi/requester")
    val PceReplyToUri = URI.create("http://example.com/pce/reply")

    def request(
        correlationId: CorrelationId,
        operation: NsiProviderOperation,
        segment: ComputedSegment = A,
        any: List[JAXBElement[_]] = Nil
    ) = {
      val headers = NsiHeaders(
        correlationId,
        AggregatorNsa,
        segment.provider.nsa,
        Some(ProviderReplyToUri),
        NsiHeaders.ProviderProtocolVersion,
        any = XmlAny(any)
      )
      ToProvider(NsiProviderMessage(headers, operation), segment.provider)
    }
    def response(
        correlationId: CorrelationId,
        operation: NsiRequesterOperation,
        any: List[JAXBElement[_]] = Nil
    ) = {
      val headers = NsiHeaders(
        correlationId,
        RequesterNsa,
        AggregatorNsa,
        None,
        NsiHeaders.RequesterProtocolVersion,
        any = XmlAny(any)
      )
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
    def timeout(
        correlationId: CorrelationId,
        originalCorrelationId: CorrelationId,
        timestamp: Instant
    ) = {
      MessageDeliveryFailure(
        correlationId,
        None,
        originalCorrelationId,
        URI.create("http://pce.local/"),
        timestamp,
        "message-delivery-timeout"
      )
    }
    def failedAck(
        correlationId: CorrelationId,
        status: Int = 400,
        statusText: String = "Bad Request",
        message: String = "Find path request not accepted"
    ) = {
      AckFromPce(PceFailed(correlationId, status, statusText, message))
    }
    def acceptedAck(correlationId: CorrelationId) = {
      AckFromPce(PceAccepted(correlationId))
    }
  }

}
