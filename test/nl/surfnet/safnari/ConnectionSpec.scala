package nl.surfnet.safnari

import java.util.UUID
import java.net.URI
import javax.xml.datatype.DatatypeFactory
import scala.collection.JavaConverters._
import org.specs2.specification.Scope
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._07.framework.types.TypeValuePairListType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import org.joda.time.DateTime
import org.joda.time.DateTimeUtils

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends helpers.Specification {
  // These tests modify global state through joda time mocking.
  sequential

  import NsiMessageSpec._

  abstract class fixture extends org.specs2.mutable.After {
    override def after = DateTimeUtils.setCurrentMillisSystem()

    val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    val Headers = NsiHeaders(CorrelationId(0, 0), "RequesterNSA", AggregatorNsa, Some(URI.create("http://example.com/")), NsiHeaders.ProviderProtocolVersion)
    val ConnectionId = "ConnectionId"
    val ReserveCorrelationId = newCorrelationId
    val CommitCorrelationId = newCorrelationId

    val InitialReserveMessage = ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)).message.asInstanceOf[NsiProviderMessage[InitialReserve]]
    val InitialReserveHeaders = InitialReserveMessage.headers
    val InitialMessages = Seq(FromRequester(InitialReserveMessage))

    val NsiReplyToUri = URI.create("http://example.com/nsi/requester")
    val PceReplyToUri = URI.create("http://example.com/pce/reply")

    def toProviderHeaders(provider: ProviderEndPoint, correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, provider.nsa, Some(NsiReplyToUri), NsiHeaders.ProviderProtocolVersion)
    def toRequesterHeaders(correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, "RequesterNSA", None, NsiHeaders.RequesterProtocolVersion)

    val connection = new ConnectionEntity(ConnectionId, InitialReserveMessage, () => newCorrelationId, AggregatorNsa, NsiReplyToUri, PceReplyToUri)
    def schedule = connection.rsm.criteria.getSchedule()

    val processInbound = new IdempotentProvider(AggregatorNsa, connection.process)

    def given(messages: Message*): Unit = messages.foreach {
      case inbound @ FromProvider(NsiRequesterMessage(_, _: NsiQueryRecursiveResponse)) =>
        connection.queryRecursiveResult(inbound)
      case inbound @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) =>
        connection.queryRecursive(inbound)
      case inbound: InboundMessage =>
        processInbound(inbound) aka s"given message $inbound must be processed" must beRight
      case outbound: OutboundMessage =>
        // FIXME compare against actual outbound messages?
        connection.process(outbound)
    }

    var messages: Seq[Message] = Nil
    def when(message: InboundMessage): Option[Seq[OutboundMessage]] = {
      messages = Nil

      val response = message match {
        case query @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) => connection.queryRecursive(query)
        case message: FromRequester =>
          val first = processInbound(message).right.toOption
          val second = processInbound(message).right.toOption
          second aka "idempotent retransmit" must beEqualTo(first)
          first
        case other: InboundMessage =>
          processInbound(message).right.toOption
      }
      response.tap(_.foreach(messages = _))
    }

    def connectionData = connection.query
    def segments = connection.segments

    def reservationState = connectionData.getConnectionStates().getReservationState()
    def provisionState = connectionData.getConnectionStates().getProvisionState()
    def lifecycleState = connectionData.getConnectionStates().getLifecycleState()
    def dataPlaneStatus = connectionData.getConnectionStates().getDataPlaneStatus()
  }

  abstract class ReservedConnection extends fixture {
    given(
      ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
      pce.confirm(CorrelationId(0, 1), A),
      upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
      ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)),
      upa.response(CorrelationId(0, 6), ReserveCommitConfirmed("ConnectionIdA")))
  }

  abstract class ReservedConnectionWithTwoSegments extends fixture {
    given(
      ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
      pce.confirm(CorrelationId(0, 1), A, B),

      upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA")),
      upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")),
      upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
      upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria)),

      ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)),
      upa.response(CorrelationId(0, 8), ReserveCommitConfirmed("ConnectionIdA")),
      upa.response(CorrelationId(0, 9), ReserveCommitConfirmed("ConnectionIdB")))
  }

  trait Released { this: ReservedConnection =>
  }

  trait ReleasedSegments { this: ReservedConnectionWithTwoSegments =>
  }

  trait Provisioned { this: ReservedConnection =>
    val ProvisionCorrelationId = newCorrelationId

    given(
      ura.request(ProvisionCorrelationId, Provision(ConnectionId)),
      upa.response(CorrelationId(0, 8), ProvisionConfirmed("ConnectionIdA")))
  }

  trait ProvisionedSegments { this: ReservedConnectionWithTwoSegments =>
    val ProvisionCorrelationId = newCorrelationId

    given(
      ura.request(ProvisionCorrelationId, Provision(ConnectionId)),
      upa.response(CorrelationId(0, 11), ProvisionConfirmed("ConnectionIdA")),
      upa.response(CorrelationId(0, 12), ProvisionConfirmed("ConnectionIdB")))
  }

  "A connection" should {
    "send a path computation request when reserve is received" in new fixture {
      when(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

      messages must contain(ToPce(PathComputationRequest(
        correlationId = CorrelationId(0, 3),
        replyTo = PceReplyToUri,
        schedule = Schedule,
        serviceType = ServiceType("ServiceType", Service))))
    }

    "send reserve request for each segment when path computation confirmed is received" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

      when(pce.confirm(CorrelationId(0, 1), A, B))

      messages must haveSize(2)
      messages must haveAllElementsLike {
        case ToProvider(NsiProviderMessage(_, reserve: InitialReserve), A.provider) => ok
        case ToProvider(NsiProviderMessage(_, reserve: InitialReserve), B.provider) => ok
      }
    }

    "fail the connection when pce did not accept the find path request" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service))
      )

      when(pce.failedAck(CorrelationId(0, 1)))

      messages must contain(like[Message] {
        case ToRequester(NsiRequesterMessage(_, ReserveFailed(_))) => ok
      }).exactly(1)
    }

    "fail the connection when path computation fails" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

      when(pce.fail(CorrelationId(0, 1), "failed"))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(Headers.copy(correlationId = ReserveCorrelationId).forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.NoPathFound.id)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "notify timeout when path computation times out" in new fixture {
      val TimeoutTimestamp = DateTime.now().plusMinutes(2)
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

      when(pce.timeout(CorrelationId(1, 1), CorrelationId(0, 2), TimeoutTimestamp))

      messages must contain(agg.notification(CorrelationId(0, 4), MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
        .withConnectionId(ConnectionId)
        .withCorrelationId(CorrelationId(0, 2).toString)
        .withNotificationId(1)
        .withTimeStamp(TimeoutTimestamp.toXmlGregorianCalendar))))
    }

    "confirm the reservation with a single path segment" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A))

      when(upa.acknowledge(CorrelationId(0, 4), ReserveResponse(ConnectionId)))
      when(upa.response(CorrelationId(0, 4), ReserveConfirmed(ConnectionId, ConfirmCriteria)))

      messages must contain(ToRequester(NsiRequesterMessage(InitialReserveHeaders.forAsyncReply, ReserveConfirmed(ConnectionId, ConfirmCriteria))))
      messages must contain(agg.response(ReserveCorrelationId, ReserveConfirmed(ConnectionId, ConfirmCriteria)))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
    }

    "notify timeout to requester when child times out" in new fixture {
      val TimeoutTimestamp = DateTime.now().plusMinutes(2)
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        agg.request(CorrelationId(0, 4), InitialReserve(InitialReserveType, ConfirmCriteria, A.serviceType.service)),
        pce.confirm(CorrelationId(0, 3), A))

      when(upa.timeout(CorrelationId(2, 7), CorrelationId(0, 4), TimeoutTimestamp))

      messages must contain(agg.notification(CorrelationId(0, 6), MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
        .withConnectionId(ConnectionId)
        .withCorrelationId(CorrelationId(0, 4).toString)
        .withNotificationId(1)
        .withTimeStamp(TimeoutTimestamp.toXmlGregorianCalendar))))
    }

    "ignore reserve response after async reserve confirm" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A),
        upa.response(CorrelationId(0, 4), ReserveConfirmed(ConnectionId, ConfirmCriteria)))

      when(upa.acknowledge(CorrelationId(0, 4), ReserveResponse(ConnectionId)))

      messages must beEmpty
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
    }

    "be in reservation held state when both segments are confirmed" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 1), A, B),
        upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA")),
        upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")))

      when(upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))
      messages must beEmpty
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, ReservationStateEnumType.RESERVE_CHECKING, _, _, _, _) => ok }

      when(upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria)))
      messages must contain(ToRequester(NsiRequesterMessage(InitialReserveHeaders.forAsyncReply, ReserveConfirmed(ConnectionId, ConfirmCriteria))))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok }
    }

    "fail the reservation with a single path segment" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A))

      when(upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthUnavailable.toServiceException(A.provider.nsa)))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(1)
          failed.getServiceException().getChildException().get(0).getErrorId() must beEqualTo(NsiError.BandwidthUnavailable.id)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "fail the reservation with two segments and at least one fails" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A, B),
        upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthUnavailable.toServiceException(A.provider.nsa)))))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(upa.response(CorrelationId(0, 5), ReserveConfirmed("connectionIdB", ConfirmCriteria)))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(1)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "fail the reservation with two segments and one downstream communication fails" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A, B),
        upa.error(CorrelationId(0, 4), new ServiceExceptionType().withText("communication error")))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(upa.response(CorrelationId(0, 5), ReserveConfirmed("connectionIdB", ConfirmCriteria)))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(1)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "fail the reservation with two segments when both fail" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A, B),
        upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthUnavailable.toServiceException(A.provider.nsa)))))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(upa.response(CorrelationId(0, 5), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdB").withServiceException(NsiError.BandwidthUnavailable.toServiceException(B.provider.nsa)))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(2)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "be in committing state when reserve commit is received" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

      when(ura.request(newCorrelationId, ReserveCommit(ConnectionId)))

      messages must haveOneElementLike { case ToProvider(NsiProviderMessage(_, _: ReserveCommit), _) => ok }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_COMMITTING)
    }

    "be in reserved state when reserve commit confirmed is received" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
        ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)))

      when(upa.response(CorrelationId(0, 6), ReserveCommitConfirmed("ConnectionIdA")))

      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = CommitCorrelationId).forAsyncReply, ReserveCommitConfirmed(ConnectionId))))
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
    }

    "reject commit when in initial state" in new fixture {
      val ack = when(ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)))

      ack must beNone
    }

    "be in aborting state when reserve abort is received" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

      when(ura.request(newCorrelationId, ReserveAbort(ConnectionId)))

      messages must haveOneElementLike { case ToProvider(NsiProviderMessage(_, _: ReserveAbort), _) => ok }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)
    }

    "be in reserve timeout state when provider times out" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
        pce.confirm(CorrelationId(0, 3), A),
        upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

      when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType().withConnectionId("ConnectionIdA"))))

      messages must contain(agg.response(ReserveCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
        .withConnectionId(ConnectionId)
        .withOriginatingConnectionId("ConnectionIdA")
        .withNotificationId(1))))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_TIMEOUT)
    }

    "provide basic information about uncommitted connections" in new fixture {
      given(InitialMessages: _*)

      val result = connection.query

      result.getConnectionId() must beEqualTo(ConnectionId)
      result.getDescription() must beEqualTo(InitialReserveType.getDescription())
      result.getGlobalReservationId() must beEqualTo(InitialReserveType.getGlobalReservationId())
      result.getCriteria() must haveSize(0)
    }

    "provide detailed information about committed connections with children" in new ReservedConnection {
      val result = connection.query

      result.getCriteria() must haveSize(1)
      val committed = result.getCriteria().get(0)
      committed.getVersion() must beEqualTo(RequestCriteria.getVersion())
      committed.getSchedule() must beEqualTo(RequestCriteria.getSchedule())
      committed.getServiceType() must beEqualTo(RequestCriteria.getServiceType())
      committed.getChildren().getChild() must haveSize(1)
    }

    "be in released state when initial reserve" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
    }

    "initialize the provisioning state machine when the path is confirmed" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

      when(pce.confirm(CorrelationId(0, 1), A))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
    }

    "become provisioning on provision request" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      when(ura.request(ProvisionCorrelationId, Provision(ConnectionId)))

      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 8)), Provision("ConnectionIdA")), A.provider))
    }

    "send a provision confirmed to requester" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      given(ura.request(ProvisionCorrelationId, Provision(ConnectionId)))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONING)

      when(upa.response(CorrelationId(0, 8), ProvisionConfirmed("ConnectionIdA")))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONED)
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = ProvisionCorrelationId).forAsyncReply, ProvisionConfirmed(ConnectionId))))
    }

    "send a provision confirmed with multiple segments" in new ReservedConnectionWithTwoSegments with ReleasedSegments {
      val ProvisionCorrelationId = newCorrelationId

      given(ura.request(ProvisionCorrelationId, Provision(ConnectionId)))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONING)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, ReservationStateEnumType.RESERVE_START, _, ProvisionStateEnumType.PROVISIONING, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, ReservationStateEnumType.RESERVE_START, _, ProvisionStateEnumType.PROVISIONING, _, _) => ok }

      when(upa.response(CorrelationId(0, 11), ProvisionConfirmed("ConnectionIdA")))

      messages must beEmpty
      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONING)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, _, _, ProvisionStateEnumType.PROVISIONED, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, _, _, ProvisionStateEnumType.PROVISIONING, _, _) => ok }

      when(upa.response(CorrelationId(0, 12), ProvisionConfirmed("ConnectionIdB")))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONED)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, _, _, ProvisionStateEnumType.PROVISIONED, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, _, _, ProvisionStateEnumType.PROVISIONED, _, _) => ok }
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = ProvisionCorrelationId).forAsyncReply, ProvisionConfirmed(ConnectionId))))
    }

    "become releasing on release request" in new ReservedConnection with Provisioned {
      val ReleaseCorrelationId = newCorrelationId

      when(ura.request(ReleaseCorrelationId, Release(ConnectionId)))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASING)
      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 10)), Release("ConnectionIdA")), A.provider))
    }

    "send release confirmed to requester" in new ReservedConnection with Provisioned {
      val ReleaseCorrelationId = newCorrelationId

      given(
        ura.request(ReleaseCorrelationId, Release(ConnectionId)),
        upa.acknowledge(CorrelationId(0, 10), GenericAck()))

      when(upa.response(CorrelationId(0, 10), ReleaseConfirmed("ConnectionIdA")))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = ReleaseCorrelationId).forAsyncReply, ReleaseConfirmed(ConnectionId))))
    }

    "reject release request when released" in new ReservedConnection with Released {
      val ack = when(ura.request(newCorrelationId, Release(ConnectionId)))

      ack must beNone
    }

    "reject provision request when provisioned" in new ReservedConnection with Provisioned {
      val ack = when(ura.request(newCorrelationId, Provision(ConnectionId)))

      ack must beNone
    }

    "become terminating on terminate request" in new ReservedConnection {
      val TerminateCorrelationId = newCorrelationId

      when(ura.request(TerminateCorrelationId, Terminate(ConnectionId)))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 8)), Terminate("ConnectionIdA")), A.provider))
    }

    "send a terminate confirmed to requester" in new ReservedConnection {
      val TerminateCorrelationId = newCorrelationId

      given(
        ura.request(TerminateCorrelationId, Terminate(ConnectionId)),
        upa.acknowledge(CorrelationId(0, 8), GenericAck()))

      when(upa.response(CorrelationId(0, 8), TerminateConfirmed("ConnectionIdA")))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = TerminateCorrelationId).forAsyncReply, TerminateConfirmed(ConnectionId))))
    }

    "send a terminate for a multi segment reservation" in new ReservedConnectionWithTwoSegments {
      val TerminateCorrelationId = newCorrelationId

      given(ura.request(TerminateCorrelationId, Terminate(ConnectionId)))

      when(upa.response(CorrelationId(0, 11), TerminateConfirmed("ConnectionIdA")))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, _, LifecycleStateEnumType.TERMINATED, _, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, _, LifecycleStateEnumType.TERMINATING, _, _, _) => ok }
      messages must beEmpty

      when(upa.response(CorrelationId(0, 12), TerminateConfirmed("ConnectionIdB")))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdA"), _, _, LifecycleStateEnumType.TERMINATED, _, _, _) => ok }
      segments must haveOneElementLike { case ConnectionData(Some("ConnectionIdB"), _, _, LifecycleStateEnumType.TERMINATED, _, _, _) => ok }
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = TerminateCorrelationId).forAsyncReply, TerminateConfirmed(ConnectionId))))
    }

    "have a data plane inactive" in new ReservedConnection with Provisioned {
      dataPlaneStatus.isActive() must beFalse
    }

    "have a data plane active on data plane change" in new ReservedConnection with Provisioned {
      when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00")))))

      dataPlaneStatus.isActive() must beTrue
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(
        agg.notification(CorrelationId(0, 10), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(1)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))
          .withDataPlaneStatus(dataPlaneStatusType(true)))))
    }

    "have a data plane active on data plane change of all segments" in new ReservedConnectionWithTwoSegments with ProvisionedSegments {
      when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00")))))

      dataPlaneStatus.isActive() must beFalse
      messages must beEmpty

      when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdB")
        .withDataPlaneStatus(dataPlaneStatusType(true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:10")))))

      dataPlaneStatus.isActive() must beTrue
      messages must contain(agg.notification(CorrelationId(0, 15), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId(ConnectionId)
        .withNotificationId(1)
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:10"))
        .withDataPlaneStatus(dataPlaneStatusType(true)))))
    }

    "have a data plane inactive on data plane change" in new ReservedConnection with Provisioned {
      given(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00")))))

      when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(false))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:15")))))

      dataPlaneStatus.isActive() must beFalse
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(
        agg.notification(CorrelationId(0, 12), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(2)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:15"))
          .withDataPlaneStatus(dataPlaneStatusType(false)))))
    }

    "become failed on ForcedEnd error event" in new ReservedConnection {
      val ProviderErrorEventCorrelationId = newCorrelationId
      val TimeStamp = org.joda.time.DateTime.now().minusMinutes(3).toXmlGregorianCalendar
      val ChildException = new ServiceExceptionType()
        .withConnectionId("ConnectionIdA")
        .withNsaId(A.provider.nsa)
        .withErrorId("FORCED_END")
        .withText("ERROR_TEXT")
        .withServiceType("SERVICE_TYPE")

      when(upa.notification(ProviderErrorEventCorrelationId, ErrorEvent(new ErrorEventType()
        .withConnectionId("ConnectionIdA")
        .withNotificationId(4)
        .withTimeStamp(TimeStamp)
        .withEvent(EventEnumType.FORCED_END)
        .withServiceException(ChildException))))

      lifecycleState must beEqualTo(LifecycleStateEnumType.FAILED)
      connection.lsm.get.childConnectionState("ConnectionIdA") must beEqualTo(LifecycleStateEnumType.FAILED)
      messages must contain(agg.notification(CorrelationId(0, 9), ErrorEvent(new ErrorEventType()
        .withConnectionId("ConnectionId")
        .withNotificationId(1)
        .withTimeStamp(TimeStamp)
        .withEvent(EventEnumType.FORCED_END)
        .withServiceException(new ServiceExceptionType()
          .withConnectionId("ConnectionId")
          .withNsaId(AggregatorNsa)
          .withErrorId("FORCED_END")
          .withText("ERROR_TEXT")
          .withServiceType("SERVICE_TYPE")
          .withChildException(ChildException)))))
    }

    "become terminating on terminate after failed" in new ReservedConnection {
      val ProviderErrorEventCorrelationId = newCorrelationId
      val TimeStamp = org.joda.time.DateTime.now().minusMinutes(3).toXmlGregorianCalendar

      given(upa.notification(ProviderErrorEventCorrelationId, ErrorEvent(new ErrorEventType()
        .withConnectionId("ConnectionIdA")
        .withNotificationId(4)
        .withTimeStamp(TimeStamp)
        .withEvent(EventEnumType.FORCED_END))))

      val TerminateCorrelationId = newCorrelationId

      when(ura.request(TerminateCorrelationId, Terminate(ConnectionId)))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 11)), Terminate("ConnectionIdA")), A.provider))
    }

    "become PassedEndTime on PassedEndTime event" in new ReservedConnection {
      DateTimeUtils.setCurrentMillisFixed(schedule.endTime.get.getMillis)

      when(PassedEndTime(CorrelationId(3, 0), connection.id, schedule.endTime.get))

      lifecycleState must beEqualTo(LifecycleStateEnumType.PASSED_END_TIME)
    }

    "ignore PassedEndTime message before scheduled end time" in new ReservedConnection {
      DateTimeUtils.setCurrentMillisFixed(schedule.endTime.get.minusMinutes(5).getMillis)

      when(PassedEndTime(CorrelationId(3, 0), connection.id, schedule.endTime.get))

      lifecycleState must beEqualTo(LifecycleStateEnumType.CREATED)
    }

    "become terminating on terminate after PassedEndTime" in new ReservedConnection {
      DateTimeUtils.setCurrentMillisFixed(schedule.endTime.get.getMillis)
      given(PassedEndTime(CorrelationId(3, 0), connection.id, schedule.endTime.get))

      when(ura.request(CorrelationId(1, 0), Terminate(ConnectionId)))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 7)), Terminate("ConnectionIdA")), A.provider))
    }

    "pass MessageDeliveryTimeout notifications to requester" in new ReservedConnection {
      val TimedOutMessageCorrelationId = newCorrelationId.toString
      val TimeStamp = org.joda.time.DateTime.now().minusMinutes(3).toXmlGregorianCalendar

      when(upa.notification(newCorrelationId, MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
        .withConnectionId("ConnectionIdA")
        .withNotificationId(12)
        .withCorrelationId(TimedOutMessageCorrelationId)
        .withTimeStamp(TimeStamp))))

      messages must contain(agg.notification(CorrelationId(0, 9), MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
        .withConnectionId(ConnectionId)
        .withNotificationId(1)
        .withCorrelationId(TimedOutMessageCorrelationId)
        .withTimeStamp(TimeStamp))))
    }

    "pass ErrorEvent notifications to requester" in new ReservedConnection {
      val TimeStamp = org.joda.time.DateTime.now().minusMinutes(3).toXmlGregorianCalendar

      when(upa.notification(newCorrelationId, ErrorEvent(new ErrorEventType()
        .withConnectionId("ConnectionIdA")
        .withNotificationId(12)
        .withEvent(EventEnumType.DATAPLANE_ERROR)
        .withTimeStamp(TimeStamp))))

      messages must contain(agg.notification(CorrelationId(0, 8), ErrorEvent(new ErrorEventType()
        .withConnectionId(ConnectionId)
        .withNotificationId(1)
        .withEvent(EventEnumType.DATAPLANE_ERROR)
        .withTimeStamp(TimeStamp))))
    }

    "pass ErrorEvent notifications to requester with child exceptions" in new ReservedConnection {
      val TimeStamp = org.joda.time.DateTime.now().minusMinutes(3).toXmlGregorianCalendar
      val ChildException = new ServiceExceptionType()
        .withConnectionId("ConnectionIdA")
        .withNsaId(A.provider.nsa)
        .withErrorId("ERROR_ID")
        .withText("TEXT")
        .withServiceType("SERVICE_TYPE")

      when(upa.notification(newCorrelationId, ErrorEvent(new ErrorEventType()
        .withConnectionId("ConnectionIdA")
        .withNotificationId(12)
        .withEvent(EventEnumType.DATAPLANE_ERROR)
        .withTimeStamp(TimeStamp)
        .withServiceException(ChildException))))

      messages must contain(agg.notification(CorrelationId(0, 8), ErrorEvent(new ErrorEventType()
        .withConnectionId(ConnectionId)
        .withNotificationId(1)
        .withEvent(EventEnumType.DATAPLANE_ERROR)
        .withTimeStamp(TimeStamp)
        .withServiceException(new ServiceExceptionType()
          .withConnectionId(ConnectionId)
          .withNsaId(AggregatorNsa)
          .withErrorId("ERROR_ID")
          .withText("TEXT")
          .withServiceType("SERVICE_TYPE")
          .withChildException(ChildException)))))
    }

    "send query recursive to all child providers" in new ReservedConnectionWithTwoSegments {
      when(ura.request(newCorrelationId, QueryRecursive(Some(Left(ConnectionId :: Nil)))))

      messages must haveSize(2)
      messages must haveAllElementsLike {
        case ToProvider(NsiProviderMessage(_, QueryRecursive(Some(Left("ConnectionIdA" :: Nil)))), A.provider) => ok
        case ToProvider(NsiProviderMessage(_, QueryRecursive(Some(Left("ConnectionIdB" :: Nil)))), B.provider) => ok
      }
    }

    "send query recursive confirm to requester when all child providers replied" in new ReservedConnectionWithTwoSegments {
      given(
        ura.request(newCorrelationId, QueryRecursive(Some(Left(ConnectionId :: Nil)))),
        upa.response(CorrelationId(0, 11), QueryRecursiveConfirmed(Nil)))

      when(upa.response(CorrelationId(0, 12), QueryRecursiveConfirmed(Nil)))

      messages must haveSize(1)
      messages must haveAllElementsLike {
        case ToRequester(NsiRequesterMessage(_, QueryRecursiveConfirmed(results))) => results must haveSize(1)
      }
    }

  }

  private def dataPlaneStatusType(active: Boolean) = new DataPlaneStatusType().withActive(active).withVersion(0).withVersionConsistent(true)
}
