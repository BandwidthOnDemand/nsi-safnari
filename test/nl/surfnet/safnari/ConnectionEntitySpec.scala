package nl.surfnet.safnari

import java.util.UUID
import java.net.URI
import javax.xml.datatype.DatatypeFactory
import scala.collection.JavaConverters._
import org.specs2.specification.Scope
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._12.framework.types.TypeValuePairListType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.joda.time.DateTime
import org.joda.time.DateTimeUtils
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionEntitySpec extends helpers.Specification {
  // These tests modify global state through joda time mocking.
  sequential

  import NsiMessageSpec._

  abstract class fixture extends org.specs2.mutable.After {
    override def after = DateTimeUtils.setCurrentMillisSystem()

    val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    val ConnectionId = "ConnectionId"
    val ReserveCorrelationId = newCorrelationId
    val CommitCorrelationId = newCorrelationId
    val AbortCorrelationId = CommitCorrelationId

    val NsiReplyToUri = URI.create("http://example.com/nsi/requester")
    val PceReplyToUri = URI.create("http://example.com/pce/reply")

    def toProviderHeaders(provider: ProviderEndPoint, correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, provider.nsa, Some(NsiReplyToUri), NsiHeaders.ProviderProtocolVersion)

    var connection: ConnectionEntity = _
    var processInbound: IdempotentProvider = _

    def schedule = connection.rsm.criteria.getSchedule()

    def given(messages: Message*): Unit = messages.foreach {
      case inbound @ FromProvider(NsiRequesterMessage(_, _: QueryRecursiveConfirmed)) =>
        connection.queryRecursiveResult(inbound)
      case inbound @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) =>
        connection.queryRecursive(inbound)
      case inbound @ FromRequester(NsiProviderMessage(headers, _: InitialReserve)) =>
        initialReserve(inbound)
      case inbound: InboundMessage =>
        processInbound(inbound) aka s"given message $inbound must be processed" must beRight
      case outbound: OutboundMessage =>
        connection.process(outbound)
    }

    private def initialReserve(reserve: FromRequester) = {
      connection = new ConnectionEntity(ConnectionId, reserve.message.asInstanceOf[NsiProviderMessage[InitialReserve]], () => newCorrelationId, AggregatorNsa, ChainAlgorithm, NsiReplyToUri, PceReplyToUri)
      processInbound = new IdempotentProvider(AggregatorNsa, connection.process)
      processInbound(reserve).right.toOption
    }

    var messages: Seq[Message] = Nil
    def when(message: InboundMessage): Option[Seq[OutboundMessage]] = {
      messages = Nil

      val response = message match {
        case query @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) => connection.queryRecursive(query)
        case reserve @ FromRequester(NsiProviderMessage(headers, _: InitialReserve)) =>
          initialReserve(reserve)
        case message: FromRequester =>
          val first = processInbound(message).right.toOption
          val second = processInbound(message).right.toOption
          second aka "idempotent retransmit" must beEqualTo(first)
          first
        case other: InboundMessage =>
          processInbound(message).right.toOption
      }
      response.tap(_.foreach { outbound =>
        // Validate outbound messages against XML schema.
        outbound.foreach {
          case ToRequester(msg) =>
            import NsiSoapConversions._
            val conversion = NsiRequesterMessageToDocument(None)(NsiRequesterOperationToElement) andThen NsiXmlDocumentConversion
            conversion.apply(msg).get
          case ToProvider(msg, _) =>
            import NsiSoapConversions._
            val conversion = NsiProviderMessageToDocument(None)(NsiProviderOperationToElement) andThen NsiXmlDocumentConversion
            conversion.apply(msg).get
          case ToPce(msg) =>
            // No schema to validate against.
        }
        messages = outbound
      })
    }

    def connectionData = connection.query
    def segments = connection.segments

    def childConnectionData(childConnectionId: ConnectionId): ConnectionData = segments.find(_.connectionId == Some(childConnectionId)).getOrElse(failure(s"no child data for $childConnectionId"))

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

  abstract class ReserveHeldConnectionWithTwoSegments extends fixture {
    given(
      ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
      pce.confirm(CorrelationId(0, 1), A, B),

      upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA")),
      upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")),
      upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
      upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria)))
  }

  abstract class ReservedConnectionWithTwoSegments extends ReserveHeldConnectionWithTwoSegments {
    given(
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

  trait DataPlaneActive { this: ReservedConnection =>
    given(
      upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))
  }

  trait PassedEndTime { this: ReservedConnection =>
    DateTimeUtils.setCurrentMillisFixed(schedule.endTime.get.getMillis)
    given(PassedEndTime(CorrelationId(3, 0), connection.id, schedule.endTime.get))
  }

  trait Failed { this: ReservedConnection =>
    val TimeStamp = org.joda.time.DateTime.now().minusMinutes(3).toXmlGregorianCalendar

    given(upa.notification(newCorrelationId, ErrorEvent(new ErrorEventType()
      .withConnectionId("ConnectionIdA")
      .withNotificationId(4)
      .withTimeStamp(TimeStamp)
      .withEvent(EventEnumType.FORCED_END))))
  }

  trait ProvisionedSegments { this: ReservedConnectionWithTwoSegments =>
    val ProvisionCorrelationId = newCorrelationId

    given(
      ura.request(ProvisionCorrelationId, Provision(ConnectionId)),
      upa.response(CorrelationId(0, 11), ProvisionConfirmed("ConnectionIdA")),
      upa.response(CorrelationId(0, 12), ProvisionConfirmed("ConnectionIdB")))
  }

  "A connection" >> {
    "in initial state" should {
      "send a path computation request when reserve is received" in new fixture {
        val connectionTrace = new ConnectionType().withIndex(0).withValue("foo") :: Nil
        when(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service), Nil, connectionTrace))

        messages must contain(ToPce(PathComputationRequest(
          correlationId = CorrelationId(0, 3),
          replyTo = PceReplyToUri,
          schedule = Schedule,
          serviceType = ServiceType("ServiceType", Service),
          algorithm = ChainAlgorithm,
          connectionTrace = connectionTrace)))
      }

      "reject commit" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

        val ack = when(ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)))

        ack must beNone
      }
    }

    "in path computation state" should {
      "send reserve request for each segment when path computation confirmed is received" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

        when(pce.confirm(CorrelationId(0, 1), A, B))

        messages must haveSize(2)
        messages must haveAllElementsLike {
          case ToProvider(NsiProviderMessage(_, InitialReserve(_, _, Service)), A.provider) => ok
          case ToProvider(NsiProviderMessage(_, InitialReserve(_, _, Service)), B.provider) => ok
        }
      }

      "send reserve request for each segment including the session security attributes" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service), SessionSecurityAttr :: Nil))

        when(pce.confirm(CorrelationId(0, 1), A, B))

        messages must haveOneElementLike {
          case ToProvider(NsiProviderMessage(headers, _), A.provider) => headers.sessionSecurityAttrs must contain(SessionSecurityAttr)
          case ToProvider(NsiProviderMessage(headers, _), B.provider) => headers.sessionSecurityAttrs must contain(SessionSecurityAttr)
        }
      }

      "send reserve request including the connection trace" in new fixture {
        val requesterTrace = new ConnectionType().withIndex(2).withValue("urn:ogf:network:someone:noId")
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service), Nil, requesterTrace :: Nil))

        when(pce.confirm(CorrelationId(0, 1), A))

        messages must haveOneElementLike {
          case ToProvider(NsiProviderMessage(headers, _), A.provider) =>
            headers.connectionTrace must haveSize(2)
            headers.connectionTrace must contain(equalTo(new ConnectionType().withIndex(3).withValue(s"$AggregatorNsa:$ConnectionId")))
            headers.connectionTrace must contain(equalTo(requesterTrace))
        }
      }

      "fail the connection when pce did not accept the find path request" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

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
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
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
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
      }
    }

    "in reserve checking state" should {
      "confirm the reservation with a single path segment" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
          pce.confirm(CorrelationId(0, 3), A))

        when(upa.acknowledge(CorrelationId(0, 4), ReserveResponse(ConnectionId)))
        when(upa.response(CorrelationId(0, 4), ReserveConfirmed(ConnectionId, ConfirmCriteria)))

        messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(ReserveCorrelationId), ReserveConfirmed(ConnectionId, ConfirmCriteria))))
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
        messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(ReserveCorrelationId), ReserveConfirmed(ConnectionId, ConfirmCriteria))))

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
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
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
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
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
          upa.error(CorrelationId(0, 4), new ServiceExceptionType()
            .withNsaId("ConnectionId")
            .withErrorId("ErrorId")
            .withText("communication error")))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("connectionIdB", ConfirmCriteria)))

        messages must haveSize(1)
        messages must haveOneElementLike {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
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
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
            failed.getServiceException().getChildException().asScala must haveSize(2)
        }
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }
    }

    "in reserve held state" should {
      "be in committing state when reserve commit is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

        when(ura.request(newCorrelationId, ReserveCommit(ConnectionId)))

        messages must haveOneElementLike { case ToProvider(NsiProviderMessage(_, _: ReserveCommit), _) => ok }
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_COMMITTING)
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

        when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId("ConnectionIdA")
          .withNotificationId(32L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withOriginatingConnectionId("OriginatingConnectionId")
          .withOriginatingNSA("OriginatingNSA"))))

        messages must contain(agg.response(ReserveCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withOriginatingConnectionId("OriginatingConnectionId")
          .withOriginatingNSA("OriginatingNSA")
          .withNotificationId(1))))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_TIMEOUT)
      }
    }

    "in reserve committing state" should {
      "be in reserved state when reserve commit confirmed is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
          ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)))

        when(upa.response(CorrelationId(0, 6), ReserveCommitConfirmed("ConnectionIdA")))

        messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(CommitCorrelationId), ReserveCommitConfirmed(ConnectionId))))
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
      }
    }

    "in reserve failed state" should {
      "be in reserve aborting state when reserve abort is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthUnavailable.toServiceException(A.provider.nsa)))))

        when(ura.request(CorrelationId(0, 6), ReserveAbort(ConnectionId)))

        messages must haveOneElementLike { case ToProvider(NsiProviderMessage(_, _: ReserveAbort), _) => ok }
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)
      }

      "be in reserve start state when reserve abort is received with a single child connection without connection id" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.acknowledge(CorrelationId(0, 4), ServiceException(NsiError.PayloadError.toServiceException(A.provider.nsa))))

        when(ura.request(CorrelationId(0, 6), ReserveAbort(ConnectionId)))

        messages must haveOneElementLike { case ToRequester(NsiRequesterMessage(_, _: ReserveAbortConfirmed)) => ok }
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
      }
    }

    "in reserve aborting state" should {
      "be in reserve start state when reserve abort confirm is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthUnavailable.toServiceException(A.provider.nsa)))),
          ura.request(CorrelationId(0, 5), ReserveAbort(ConnectionId)))

        when(upa.response(CorrelationId(0, 6), ReserveAbortConfirmed("ConnectionIdA")))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
        childConnectionData("ConnectionIdA").reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)

        messages must haveOneElementLike { case ToRequester(NsiRequesterMessage(_, operation: ReserveAbortConfirmed)) => operation.connectionId must beEqualTo(ConnectionId) }
      }

      "be in reserve start state when reserve abort confirm is received from two two children" in new ReserveHeldConnectionWithTwoSegments {
        given(ura.request(AbortCorrelationId, ReserveAbort(ConnectionId)))

        when(upa.response(CorrelationId(0, 8), ReserveAbortConfirmed("ConnectionIdA")))
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)
        childConnectionData("ConnectionIdA").reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
        childConnectionData("ConnectionIdB").reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)

        when(upa.response(CorrelationId(0, 9), ReserveAbortConfirmed("ConnectionIdB")))
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
        childConnectionData("ConnectionIdA").reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
        childConnectionData("ConnectionIdB").reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)

        messages must haveOneElementLike { case ToRequester(NsiRequesterMessage(_, operation: ReserveAbortConfirmed)) => operation.connectionId must beEqualTo(ConnectionId) }
      }
    }

    "that is not yet committed" should {
      "provide basic information about uncommitted connections" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, ConfirmCriteria, Service)))

        val result = connection.query

        result.getConnectionId() must beEqualTo(ConnectionId)
        result.getDescription() must beEqualTo(InitialReserveType.getDescription())
        result.getGlobalReservationId() must beEqualTo(InitialReserveType.getGlobalReservationId())
        result.getCriteria() must haveSize(0)
      }
    }

    "that has been committed" should {
      "provide detailed information about committed connections with children" in new ReservedConnection {
        val result = connection.query

        result.getCriteria() must haveSize(1)
        val committed = result.getCriteria().get(0)
        committed.getVersion() must beEqualTo(RequestCriteria.getVersion())
        committed.getSchedule() must beEqualTo(RequestCriteria.getSchedule())
        committed.getServiceType() must beEqualTo(RequestCriteria.getServiceType())
        committed.getChildren().getChild() must haveSize(1)
      }

      "be in terminating state when terminate is received" in new ReservedConnection {
        val TerminateCorrelationId = newCorrelationId

        when(ura.request(TerminateCorrelationId, Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        childConnectionData("ConnectionIdA").lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 8)), Terminate("ConnectionIdA")), A.provider))
      }

      "be in terminating state when terminate is received (multi segment)" in new ReservedConnectionWithTwoSegments {
        val TerminateCorrelationId = newCorrelationId

        when(ura.request(TerminateCorrelationId, Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        childConnectionData("ConnectionIdA").lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
        childConnectionData("ConnectionIdB").lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        messages must contain(exactly[Message](
            ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 11)), Terminate("ConnectionIdA")), A.provider),
            ToProvider(NsiProviderMessage(toProviderHeaders(B.provider, CorrelationId(0, 12)), Terminate("ConnectionIdB")), B.provider)))
      }
    }

    "in provisioned state" should {
      "have data plane active when data plane state change is received" in new ReservedConnection with Provisioned {
        when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId("ConnectionIdA")
          .withDataPlaneStatus(dataPlaneStatusType(true))
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))

        dataPlaneStatus.isActive() must beTrue
        dataPlaneStatus.isVersionConsistent() must beTrue
        dataPlaneStatus.getVersion() must beEqualTo(ConfirmedCriteriaVersion)

        messages must contain(
          agg.notification(CorrelationId(0, 10), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
            .withConnectionId(ConnectionId)
            .withNotificationId(1)
            .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
            .withDataPlaneStatus(dataPlaneStatusType(true)))))
      }

      "have data plane active when data plane state change is received (multi segement)" in new ReservedConnectionWithTwoSegments with ProvisionedSegments {
        when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId("ConnectionIdA")
          .withDataPlaneStatus(dataPlaneStatusType(true))
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))

        dataPlaneStatus.isActive() must beFalse
        messages must beEmpty

        when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId("ConnectionIdB")
          .withDataPlaneStatus(dataPlaneStatusType(true))
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:10:00Z")))))

        dataPlaneStatus.isActive() must beTrue
        messages must contain(agg.notification(CorrelationId(0, 15), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(1)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:10:00Z"))
          .withDataPlaneStatus(dataPlaneStatusType(true)))))
      }

      "be in terminating state when terminate is received" in new ReservedConnection with Provisioned with DataPlaneActive {
        val TerminateCorrelationId = newCorrelationId

        when(ura.request(TerminateCorrelationId, Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        childConnectionData("ConnectionIdA").lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 12)), Terminate("ConnectionIdA")), A.provider))
      }
    }

    "in terminating state" should {
      "send a terminate confirmed to requester when terminate confirmed is received" in new ReservedConnection {
        val TerminateCorrelationId = newCorrelationId

        given(
          ura.request(TerminateCorrelationId, Terminate(ConnectionId)),
          upa.acknowledge(CorrelationId(0, 8), GenericAck()))

        when(upa.response(CorrelationId(0, 8), TerminateConfirmed("ConnectionIdA")))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
        messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(TerminateCorrelationId), TerminateConfirmed(ConnectionId))))
      }

      "send a terminate confirmed to requester when terminate confirmed for each segment is received" in new ReservedConnectionWithTwoSegments {
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
        messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(TerminateCorrelationId), TerminateConfirmed(ConnectionId))))
      }

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

    "in released state" should {
      "become provisioning on provision request" in new ReservedConnection with Released {
        when(ura.request(newCorrelationId, Provision(ConnectionId)))

        messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 8)), Provision("ConnectionIdA")), A.provider))
      }
    }

    "in passed end time state" should {
      "be terminating when terminate request is received" in new ReservedConnection with PassedEndTime {
        when(ura.request(newCorrelationId, Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
        childConnectionData("ConnectionIdA").lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 8)), Terminate("ConnectionIdA")), A.provider))
      }
    }

    "in failed state" should {
      "be terminating when terminate request is received" in new ReservedConnection with Failed {
        when(ura.request(newCorrelationId, Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
        childConnectionData("ConnectionIdA").lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 11)), Terminate("ConnectionIdA")), A.provider))
      }
    }

    "send a provision confirmed to requester" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      given(ura.request(ProvisionCorrelationId, Provision(ConnectionId)))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONING)

      when(upa.response(CorrelationId(0, 8), ProvisionConfirmed("ConnectionIdA")))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONED)
      messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(ProvisionCorrelationId), ProvisionConfirmed(ConnectionId))))
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
      messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(ProvisionCorrelationId), ProvisionConfirmed(ConnectionId))))
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
      messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(ReleaseCorrelationId), ReleaseConfirmed(ConnectionId))))
    }

    "reject release request when released" in new ReservedConnection with Released {
      val ack = when(ura.request(newCorrelationId, Release(ConnectionId)))

      ack must beNone
    }

    "reject provision request when provisioned" in new ReservedConnection with Provisioned {
      val ack = when(ura.request(newCorrelationId, Provision(ConnectionId)))

      ack must beNone
    }

    "have a data plane inactive" in new ReservedConnection with Provisioned {
      dataPlaneStatus.isActive() must beFalse
    }

    "have a data plane inactive on data plane change" in new ReservedConnection with Provisioned {
      given(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))

      when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(false))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:15:00Z")))))

      dataPlaneStatus.isActive() must beFalse
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(
        agg.notification(CorrelationId(0, 12), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(2)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:15:00Z"))
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
        .withOriginatingConnectionId("OriginatingConnectionId")
        .withOriginatingNSA("OriginatingNSA")
        .withServiceException(ChildException))))

      lifecycleState must beEqualTo(LifecycleStateEnumType.FAILED)
      connection.lsm.get.childConnectionState("ConnectionIdA") must beEqualTo(LifecycleStateEnumType.FAILED)
      messages must contain(agg.notification(CorrelationId(0, 9), ErrorEvent(new ErrorEventType()
        .withConnectionId("ConnectionId")
        .withNotificationId(1)
        .withTimeStamp(TimeStamp)
        .withEvent(EventEnumType.FORCED_END)
        .withOriginatingConnectionId("OriginatingConnectionId")
        .withOriginatingNSA("OriginatingNSA")
        .withServiceException(new ServiceExceptionType()
          .withConnectionId("ConnectionId")
          .withNsaId(AggregatorNsa)
          .withErrorId("FORCED_END")
          .withText("ERROR_TEXT")
          .withServiceType("SERVICE_TYPE")
          .withChildException(ChildException)))))
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
        .withOriginatingConnectionId("OriginatingConnectionId")
        .withOriginatingNSA("OriginatingNSA")
        .withTimeStamp(TimeStamp))))

      messages must contain(agg.notification(CorrelationId(0, 8), ErrorEvent(new ErrorEventType()
        .withConnectionId(ConnectionId)
        .withNotificationId(1)
        .withEvent(EventEnumType.DATAPLANE_ERROR)
        .withOriginatingConnectionId("OriginatingConnectionId")
        .withOriginatingNSA("OriginatingNSA")
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
        .withOriginatingConnectionId("OriginatingConnectionId")
        .withOriginatingNSA("OriginatingNSA")
        .withTimeStamp(TimeStamp)
        .withServiceException(ChildException))))

      messages must contain(agg.notification(CorrelationId(0, 8), ErrorEvent(new ErrorEventType()
        .withConnectionId(ConnectionId)
        .withNotificationId(1)
        .withEvent(EventEnumType.DATAPLANE_ERROR)
        .withOriginatingConnectionId("OriginatingConnectionId")
        .withOriginatingNSA("OriginatingNSA")
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

  private def dataPlaneStatusType(active: Boolean) = new DataPlaneStatusType().withActive(active).withVersion(ConfirmedCriteriaVersion).withVersionConsistent(true)
}
