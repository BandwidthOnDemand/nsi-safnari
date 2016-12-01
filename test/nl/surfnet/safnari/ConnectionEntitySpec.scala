package nl.surfnet.safnari

import java.time.{ Clock, Instant, ZoneId }
import java.time.temporal._
import javax.xml.datatype.{ DatatypeFactory, XMLGregorianCalendar }

import net.nordu.namespaces._2013._12.gnsbod.{ ConnectionTraceType, ConnectionType, ObjectFactory => ConnectionTraceTypeOF }
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.TypeValueType

import scala.collection.JavaConverters._

import nl.surfnet.bod.nsi.Nillable
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._
import helpers.NsiMessages._


@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionEntitySpec extends helpers.ConnectionEntitySpecification {
  "A connection" >> {
    "in initial state" should {
      "send a path computation request when reserve is received" in new fixture {
        val connectionTrace = new ConnectionType().withIndex(0).withValue("foo")
        when(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), Nil, new ConnectionTraceTypeOF().createConnectionTrace(new ConnectionTraceType().withConnection(connectionTrace)) :: Nil))

        messages must contain(ToPce(PathComputationRequest(
          correlationId = CorrelationId(0, 3),
          replyTo = PceReplyToUri,
          startTime = Schedule.startTime.toOption(None),
          endTime = Schedule.endTime.toOption(None),
          serviceType = ServiceType("ServiceType", Service),
          algorithm = PathComputationAlgorithm.Chain,
          connectionTrace = connectionTrace :: Nil)))
      }

      "reject commit" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        val ack = when(ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)))

        ack must beNone
      }

      "terminate immediately when no child connections have been reserved yet" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        when(ura.request(CorrelationId(2, 1), Terminate(ConnectionId)))

        messages must have size (1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, _: TerminateConfirmed)) => ok
        })
        lifecycleState must_== LifecycleStateEnumType.TERMINATED

        when(pce.confirm(CorrelationId(0, 1), A))

        messages must haveSize(0)
      }
    }

    "in path computation state" should {
      "send reserve request for each segment when path computation confirmed is received" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        when(pce.confirm(CorrelationId(0, 1), A, B))

        messages must haveSize(2)
        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(_, reserve @ InitialReserve(_)), provider) if provider == A.provider =>
            reserve.service must beSome(A.serviceType.service)
          case ToProvider(NsiProviderMessage(_, reserve @ InitialReserve(_)), provider) if provider == B.provider =>
            reserve.service must beSome(B.serviceType.service)
        }).forall
      }

      "send reserve request for each segment including the session security attributes" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), SessionSecurityAttr :: Nil))

        when(pce.confirm(CorrelationId(0, 1), A, B))

        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(headers, _), provider) if provider == A.provider =>
            headers.sessionSecurityAttrs must contain(SessionSecurityAttr)
          case ToProvider(NsiProviderMessage(headers, _), provider) if provider == B.provider =>
            headers.sessionSecurityAttrs must contain(SessionSecurityAttr)
        })
      }

      "send reserve request including the connection trace" in new fixture {
        val requesterTrace = new ConnectionTraceTypeOF().createConnectionTrace(new ConnectionTraceType().withConnection(
            new ConnectionType().withIndex(2).withValue("urn:ogf:network:someone:noId")
        ))
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), Nil, requesterTrace :: Nil))

        when(pce.confirm(CorrelationId(0, 1), A))

        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(headers, _), provider) if provider == A.provider =>
            headers.connectionTrace must haveSize(2)
            headers.connectionTrace must contain(equalTo(new ConnectionType().withIndex(3).withValue(s"$AggregatorNsa:$ConnectionId")))
            headers.connectionTrace must contain(equalTo(requesterTrace.getValue.getConnection.get(0)))
        })
      }

      "fail the connection when pce did not accept the find path request" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        when(pce.failedAck(CorrelationId(0, 1)))

        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, ReserveFailed(_))) => ok
        }).exactly(1)
      }

      "fail the connection when path computation fails" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        when(pce.fail(CorrelationId(0, 1), NsiError.NoServicePlanePathFound))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo(NsiError.NoServicePlanePathFound.id)
        })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }

      "notify timeout when path computation times out" in new fixture {
        val TimeoutTimestamp = Instant.now().plus(2, ChronoUnit.MINUTES)
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        when(pce.timeout(CorrelationId(1, 1), CorrelationId(0, 2), TimeoutTimestamp))

        messages must contain(agg.notification(CorrelationId(0, 4), MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withCorrelationId(CorrelationId(0, 2).toString)
          .withNotificationId(1)
          .withTimeStamp(TimeoutTimestamp.toXMLGregorianCalendar()))))
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
      }
    }

    "in reserve checking state" should {
      "confirm the reservation with a single path segment" in new fixture {
        val ConfirmCriteriaWithQualifiedStps = ConfirmCriteria.withPointToPointService(Service.withSourceSTP("networkId:A?vlan=1").withDestSTP("networkId:B?vlan=2"))

        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A))

        when(upa.acknowledge(CorrelationId(0, 4), ReserveResponse(ConnectionId)))
        when(upa.response(CorrelationId(0, 4), ReserveConfirmed(ConnectionId, ConfirmCriteriaWithQualifiedStps)))

        messages must contain(agg.response(ReserveCorrelationId, ReserveConfirmed(ConnectionId, ConfirmCriteriaWithQualifiedStps)))

        childConnectionData(ConnectionId).sourceStp must beEqualTo("networkId:A?vlan=1")
        childConnectionData(ConnectionId).destinationStp must beEqualTo("networkId:B?vlan=2")
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
      }

      "notify timeout to requester when child times out" in new fixture {
        val TimeoutTimestamp = Instant.now().plus(2, ChronoUnit.MINUTES)
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          agg.request(CorrelationId(0, 4), InitialReserve(InitialReserveType.tap(_.getCriteria.withPointToPointService(A.serviceType.service)))),
          pce.confirm(CorrelationId(0, 3), A))

        when(upa.timeout(CorrelationId(2, 7), CorrelationId(0, 4), TimeoutTimestamp))

        messages must contain(agg.notification(CorrelationId(0, 6), MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withCorrelationId(CorrelationId(0, 4).toString)
          .withNotificationId(1)
          .withTimeStamp(TimeoutTimestamp.toXMLGregorianCalendar()))))
      }

      "ignore reserve response after async reserve confirm" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A),
          upa.response(CorrelationId(0, 4), ReserveConfirmed(ConnectionId, ConfirmCriteria)))

        when(upa.acknowledge(CorrelationId(0, 4), ReserveResponse(ConnectionId)))

        messages must beEmpty
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
      }

      "be in reservation held state when both segments are confirmed" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A, B),
          upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA")),
          upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")))

        when(upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria.withPointToPointService(A.serviceType.service.withSourceSTP("networkId:A:A?vlan=3").withDestSTP("networkId:A:B?vlan=12")))))
        messages must beEmpty
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok })
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, ReservationStateEnumType.RESERVE_CHECKING, _, _, _, _) => ok })

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria.withPointToPointService(A.serviceType.service.withSourceSTP("networkId:B:A?vlan=12").withDestSTP("networkId:B:B?vlan=23")))))

        messages must contain(agg.response(ReserveCorrelationId, ReserveConfirmed(ConnectionId, ConfirmCriteria.withPointToPointService(A.serviceType.service.withSourceSTP("networkId:A:A?vlan=3").withDestSTP("networkId:B:B?vlan=23")))))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok })
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok })
      }

      "fail the reservation with a single path segment" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A))

        when(upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)))))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo(NsiError.CAPACITY_UNAVAILABLE.id)
            failed.getServiceException().getNsaId() must beEqualTo(AggregatorNsa)
            failed.getServiceException().getChildException().asScala must haveSize(1)
            failed.getServiceException().getChildException().get(0).getErrorId() must beEqualTo(NsiError.CAPACITY_UNAVAILABLE.id)
        })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }

      "fail the reservation with two segments and at least one fails" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)))))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("connectionIdB", ConfirmCriteria)))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo(NsiError.CAPACITY_UNAVAILABLE.id)
            failed.getServiceException().getChildException().asScala must haveSize(1)
        })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }

      "fail the reservation with two segments and one downstream communication fails" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.error(CorrelationId(0, 4), new ServiceExceptionType()
            .withNsaId("ConnectionId")
            .withErrorId("ErrorId")
            .withText("communication error")))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("connectionIdB", ConfirmCriteria)))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo("ErrorId")
            failed.getServiceException().getChildException().asScala must haveSize(1)
        })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }

      "fail the reservation with two segments when both fail" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)))))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

        when(upa.response(CorrelationId(0, 5), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdB").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(B.provider.nsa)))))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo(NsiError.CAPACITY_UNAVAILABLE.id)
            failed.getServiceException().getChildException().asScala must haveSize(2)
        })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }

      "terminate the child connection when terminating and the child connectionId is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

        when(ura.request(CorrelationId(1, 0), Terminate(ConnectionId)))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(_, body: Terminate), _) =>
            body.connectionId must_== "ConnectionIdA"
        })
        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria)))

        messages must haveSize(2)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, body: ReserveConfirmed)) =>
            body.connectionId must_== "ConnectionId"
        })
        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(_, body: Terminate), _) =>
            body.connectionId must_== "ConnectionIdB"
        })
        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      }

      "terminate the connection when first child connection is FORCED_END when second child connectionId is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
          upa.notification(CorrelationId(1, 0), ErrorEvent(new ErrorEventType().withConnectionId("ConnectionIdA").withEvent(EventEnumType.FORCED_END))))

        when(ura.request(CorrelationId(1, 0), Terminate(ConnectionId)))

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(_, body: Terminate), _) =>
            body.connectionId must_== "ConnectionIdA"
        })
        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria)))

        messages must haveSize(2)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, body: ReserveConfirmed)) =>
            body.connectionId must_== "ConnectionId"
        })
        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(_, body: Terminate), _) =>
            body.connectionId must_== "ConnectionIdB"
        })
        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      }

      "terminate the reservation when downstream initial reserve communication fails" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A),
          upa.error(CorrelationId(0, 4), new ServiceExceptionType()
            .withNsaId("ConnectionId")
            .withErrorId("ErrorId")
            .withText("communication error")))

        when(ura.request(CorrelationId(1, 0), Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
      }

      "pass child reserve timeout to requester" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A),
          upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA"))
        )

        when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId("ConnectionIdA")
          .withNotificationId(32L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(120)
          .withOriginatingConnectionId("OriginatingConnectionId-A")
          .withOriginatingNSA("OriginatingNSA-A"))))

        messages must contain(agg.notification(CorrelationId(0, 7), ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(1L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(120)
          .withOriginatingConnectionId("OriginatingConnectionId-A")
          .withOriginatingNSA("OriginatingNSA-A"))))
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
        childConnectionData("ConnectionIdA").reservationState aka "child A reservation state" must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
      }

      "pass child reserve timeout to requester for each segment" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
          upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB"))
        )

        when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId("ConnectionIdA")
          .withNotificationId(32L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(120)
          .withOriginatingConnectionId("OriginatingConnectionId-A")
          .withOriginatingNSA("OriginatingNSA-A"))))

        messages must contain(agg.notification(CorrelationId(0, 9), ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(1L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(120)
          .withOriginatingConnectionId("OriginatingConnectionId-A")
          .withOriginatingNSA("OriginatingNSA-A"))))
        childConnectionData("ConnectionIdA").reservationState aka "child A reservation state" must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

        when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId("ConnectionIdB")
          .withNotificationId(32L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:15:00Z"))
          .withTimeoutValue(180)
          .withOriginatingConnectionId("OriginatingConnectionId-B")
          .withOriginatingNSA("OriginatingNSA-B"))))

        messages must contain(agg.notification(CorrelationId(0, 11), ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(2L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:15:00Z"))
          .withTimeoutValue(180)
          .withOriginatingConnectionId("OriginatingConnectionId-B")
          .withOriginatingNSA("OriginatingNSA-B"))))
        childConnectionData("ConnectionIdB").reservationState aka "child B reservation state" must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
      }

      "confirm reservation on B confirmed when A is timed out" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
          upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB"))
        )

        when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId("ConnectionIdA")
          .withNotificationId(32L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(120)
          .withOriginatingConnectionId("OriginatingConnectionId-A")
          .withOriginatingNSA("OriginatingNSA-A"))))
        childConnectionData("ConnectionIdA").reservationState aka "child A reservation state" must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

        when(upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria)))

        childConnectionData("ConnectionIdB").reservationState aka "child B reservation state" must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_TIMEOUT)

        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, body: ReserveConfirmed)) =>
            body.connectionId must_== "ConnectionId"
        })
      }

      "with sequential routing" should {
        abstract class SequentialRoutingFixture extends fixture {
          override def pathComputationAlgorithm = PathComputationAlgorithm.Sequential
        }

        "confirm the reservation with a single path segment with sequential routing" in new SequentialRoutingFixture {
          val ConfirmCriteriaWithQualifiedStps = ConfirmCriteria.withPointToPointService(Service.withSourceSTP("networkId:A?vlan=1").withDestSTP("networkId:B?vlan=2"))

          given(
            ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
            pce.confirm(CorrelationId(0, 3), A))

          when(upa.acknowledge(CorrelationId(0, 4), ReserveResponse(ConnectionId)))
          when(upa.response(CorrelationId(0, 4), ReserveConfirmed(ConnectionId, ConfirmCriteriaWithQualifiedStps)))

          messages must contain(agg.response(ReserveCorrelationId, ReserveConfirmed(ConnectionId, ConfirmCriteriaWithQualifiedStps)))

          childConnectionData(ConnectionId).sourceStp must beEqualTo("networkId:A?vlan=1")
          childConnectionData(ConnectionId).destinationStp must beEqualTo("networkId:B?vlan=2")
          reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
        }

        "be in reservation held state when both segments are confirmed" in new SequentialRoutingFixture {
          given(
            ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
            pce.confirm(CorrelationId(0, 1), A, B),
            upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA")))

          segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, ReservationStateEnumType.RESERVE_CHECKING, _, _, _, _) => ok })

          when(upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria.withPointToPointService(A.serviceType.service.withSourceSTP("networkId:A:A?vlan=3").withDestSTP("networkId:A:B?vlan=99")))))

          reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
          messages must contain(beLike[Message] {
            case ToProvider(NsiProviderMessage(_, reserve @ InitialReserve(_)), provider) if provider == B.provider =>
              reserve.service must beSome.which((x: P2PServiceBaseType) => x.getSourceSTP() must_== "X?vlan=99")
          })

          when(upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")))

          reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
          segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok })
          segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, ReservationStateEnumType.RESERVE_CHECKING, _, _, _, _) => ok })

          when(upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria.withPointToPointService(A.serviceType.service.withSourceSTP("networkId:B:A?vlan=99").withDestSTP("networkId:B:B?vlan=23")))))

          messages must contain(agg.response(ReserveCorrelationId, ReserveConfirmed(ConnectionId, ConfirmCriteria.withPointToPointService(A.serviceType.service.withSourceSTP("networkId:A:A?vlan=3").withDestSTP("networkId:B:B?vlan=23")))))

          reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
          segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok })
          segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, ReservationStateEnumType.RESERVE_HELD, _, _, _, _) => ok })
        }

        "immediately fail the reservation with two segments when the first one fails" in new SequentialRoutingFixture {
          given(
            ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
            pce.confirm(CorrelationId(0, 3), A, B))

          when(upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)))))

          messages must haveSize(1)
          messages must contain(like[Message] {
            case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
              headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
              failed.getConnectionId() must beEqualTo(ConnectionId)
              failed.getServiceException().getErrorId() must beEqualTo(NsiError.CAPACITY_UNAVAILABLE.id)
              failed.getServiceException().getChildException().asScala must haveSize(1)
          })
          reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
        }

        "terminate the reservation when downstream initial reserve communication fails" in new SequentialRoutingFixture {
          given(
            ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
            pce.confirm(CorrelationId(0, 3), A, B))

          when(upa.error(CorrelationId(0, 4), new ServiceExceptionType()
              .withNsaId("ConnectionIdA")
              .withErrorId("ErrorId")
              .withText("communication error")))

          reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)

          when(ura.request(CorrelationId(1, 0), Terminate(ConnectionId)))

          lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
        }
      }
    }

    "in reserve held state" should {
      "be in committing state when reserve commit is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

        when(ura.request(newCorrelationId, ReserveCommit(ConnectionId)))

        messages must contain(like[Message] { case ToProvider(NsiProviderMessage(_, _: ReserveCommit), _) => ok })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_COMMITTING)
      }

      "be in aborting state when reserve abort is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

        when(ura.request(newCorrelationId, ReserveAbort(ConnectionId)))

        messages must contain(like[Message] { case ToProvider(NsiProviderMessage(_, _: ReserveAbort), _) => ok })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)
      }

      "be in reserve timeout state when provider times out" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A),
          upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)))

        when(upa.notification(newCorrelationId, ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId("ConnectionIdA")
          .withNotificationId(32L)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(180)
          .withOriginatingConnectionId("OriginatingConnectionId")
          .withOriginatingNSA("OriginatingNSA"))))

        messages must contain(agg.response(CorrelationId(0, 7), ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(1)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
          .withTimeoutValue(180)
          .withOriginatingConnectionId("OriginatingConnectionId")
          .withOriginatingNSA("OriginatingNSA"))))

        childConnectionData("ConnectionIdA").reservationState aka "child A reservation state" must beEqualTo(ReservationStateEnumType.RESERVE_TIMEOUT)

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_TIMEOUT)
      }
    }

    "in reserve committing state" should {
      "be in reserved state when reserve commit confirmed is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
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
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(2000).toServiceException(A.provider.nsa)))))

        when(ura.request(CorrelationId(0, 6), ReserveAbort(ConnectionId)))

        messages must contain(like[Message] { case ToProvider(NsiProviderMessage(_, _: ReserveAbort), _) => ok })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)
      }

      "be in reserve start state when reserve abort is received with a single child connection without connection id" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.acknowledge(CorrelationId(0, 4), ServiceException(NsiError.GenericMessagePayloadError.toServiceException(A.provider.nsa))))

        when(ura.request(CorrelationId(0, 6), ReserveAbort(ConnectionId)))

        messages must contain(like[Message] { case ToRequester(NsiRequesterMessage(_, _: ReserveAbortConfirmed)) => ok })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
      }
    }

    "in reserve aborting state" should {
      "be in reserve start state when reserve abort confirm is received" in new fixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 4), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)))),
          ura.request(CorrelationId(0, 5), ReserveAbort(ConnectionId)))

        when(upa.response(CorrelationId(0, 6), ReserveAbortConfirmed("ConnectionIdA")))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
        childConnectionData("ConnectionIdA").reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)

        messages must contain(like[Message] { case ToRequester(NsiRequesterMessage(_, operation: ReserveAbortConfirmed)) => operation.connectionId must beEqualTo(ConnectionId) })
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

        messages must contain(like[Message] { case ToRequester(NsiRequesterMessage(_, operation: ReserveAbortConfirmed)) => operation.connectionId must beEqualTo(ConnectionId) })
      }
    }

    "that is not yet committed" should {
      "provide basic information about uncommitted connections" in new fixture {
        given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

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

      "become reserve checking when modify is received" in new ReservedConnection {
        when(ura.request(ModifyCorrelationId, ModifyReserve(ModifyReserveType)))

        reservationState must_== ReservationStateEnumType.RESERVE_CHECKING

        messages must contain(exactly[Message](
          ToProvider(
            NsiProviderMessage(
              toProviderHeaders(A.provider, CorrelationId(0, 7)),
              ModifyReserve(
                new ReserveType().withConnectionId("ConnectionIdA").withCriteria(
                  new ReservationRequestCriteriaType()
                    .withVersion(4)
                    .withModifiedCapacity(500)
                    .withSchedule(new ScheduleType().withStartTime(Nillable.absent[XMLGregorianCalendar]))
                )
              )
            ),
            A.provider
          )
        ))
      }

      "fail modify when requested version is less than committed version" in new ReservedConnection {
        when(ura.request(ModifyCorrelationId, ModifyReserve(ModifyReserveType.tap(_.getCriteria.setVersion(3)))))

        reservationState must_== ReservationStateEnumType.RESERVE_FAILED

        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, ReserveFailed(failed))) =>
            failed.getServiceException.getErrorId must_== NsiError.GenericMessagePayloadError.id
        })
      }

      "fail modify when unmodifiable parameter is provided" in new ReservedConnection {
        val modify = ModifyReserveType.tap(_.getCriteria.withModifiedParameters(new TypeValueType().withType(PATH_COMPUTATION_ALGORITHM_PARAMETER_TYPE).withValue(PathComputationAlgorithm.Tree.name)))

        when(ura.request(ModifyCorrelationId, ModifyReserve(modify)))

        reservationState must_== ReservationStateEnumType.RESERVE_FAILED
      }
    }

    "in modifying state" should {
      "become reserve held when modify is confirmed" in new ReservedConnection with Modified {
        when(upa.response(CorrelationId(0, 7), ReserveConfirmed("ConnectionIdA", ModifyReserveType.getCriteria.toModifiedConfirmCriteria(connection.rsm.committedCriteria.get).get)))

        reservationState must_== ReservationStateEnumType.RESERVE_HELD

        messages must contain(exactly[Message](
          agg.response(ModifyCorrelationId, ReserveConfirmed(connection.id, ModifyReserveType.getCriteria.toModifiedConfirmCriteria(connection.rsm.committedCriteria.get).get))))
      }

      "become reserve committing when confirmed modification is committed" in new ReservedConnection with Modified {
        val ModifyCommitCorrelationId = newCorrelationId
        given(
          upa.response(CorrelationId(0, 7), ReserveConfirmed("ConnectionIdA", ModifyReserveType.getCriteria.toModifiedConfirmCriteria(connection.rsm.committedCriteria.get).get)))

        when(ura.request(ModifyCommitCorrelationId, ReserveCommit(connection.id)))

        reservationState must_== ReservationStateEnumType.RESERVE_COMMITTING

        messages must contain(exactly[Message](
          ToProvider(NsiProviderMessage(
            toProviderHeaders(A.provider, CorrelationId(0, 9)), ReserveCommit("ConnectionIdA")),
            A.provider)))
      }

      "become committed when modify commit is confirmed" in new ReservedConnection with Modified {
        val ModifyCommitCorrelationId = newCorrelationId
        given(
          upa.response(CorrelationId(0, 7), ReserveConfirmed("ConnectionIdA", ModifyReserveType.getCriteria.toModifiedConfirmCriteria(connection.rsm.committedCriteria.get).get)),
          ura.request(ModifyCommitCorrelationId, ReserveCommit(connection.id)))

        when(
          upa.response(CorrelationId(0, 9), ReserveCommitConfirmed("ConnectionIdA")))

        reservationState must_== ReservationStateEnumType.RESERVE_START

        messages must contain(exactly[Message](
          agg.response(ModifyCommitCorrelationId, ReserveCommitConfirmed(connection.id))))

        connection.rsm.pendingCriteria must beNone
        connection.rsm.committedCriteria must beSome
        connection.query.getCriteria.get(0).pointToPointService.map(_.getCapacity) must beSome(500)
      }

      "allow aborting modified reservation before commit" in new ReservedConnection with Modified {
        val AbortModifyCorrelationId = newCorrelationId
        given(
          upa.response(CorrelationId(0, 7), ReserveConfirmed("ConnectionIdA", ModifyReserveType.getCriteria.toModifiedConfirmCriteria(connection.rsm.committedCriteria.get).get)))

        when(ura.request(AbortModifyCorrelationId, ReserveAbort(connection.id))) must beSome

        reservationState must_== ReservationStateEnumType.RESERVE_ABORTING
        messages must contain(exactly[Message](
          ToProvider(NsiProviderMessage(
            toProviderHeaders(A.provider, CorrelationId(0, 9)),
            ReserveAbort("ConnectionIdA")),
            A.provider)))

        when(upa.response(CorrelationId(0, 9), ReserveAbortConfirmed("ConnectionIdA"))) must beSome
      }

      "allow modify after aborting previous modify" in new ReservedConnection with Modified {
        val AbortModifyCorrelationId = newCorrelationId
        val NewModifyCorrelationId = newCorrelationId
        given(
          upa.response(CorrelationId(0, 7), ReserveConfirmed("ConnectionIdA", ModifyReserveType.getCriteria.toModifiedConfirmCriteria(connection.rsm.committedCriteria.get).get)),
          ura.request(AbortModifyCorrelationId, ReserveAbort(connection.id)),
          agg.request(CorrelationId(0, 10), ReserveAbort("ConnectionIdA"), A),
          upa.response(CorrelationId(0, 10), ReserveAbortConfirmed("ConnectionIdA")))

        when(ura.request(NewModifyCorrelationId, ModifyReserve(ModifyReserveType))) must beSome

        reservationState must_== ReservationStateEnumType.RESERVE_CHECKING
        messages must contain(exactly[Message](
          ToProvider(NsiProviderMessage(
            toProviderHeaders(A.provider, CorrelationId(0, 11)),
            ModifyReserve(
              new ReserveType().withConnectionId("ConnectionIdA").withCriteria(
                new ReservationRequestCriteriaType()
                  .withVersion(4)
                  .withModifiedCapacity(500)
                  .withSchedule(new ScheduleType().withStartTime(Nillable.absent[XMLGregorianCalendar]))
              ))),
            A.provider)))
      }

      "become failed when modify is declined" in new ReservedConnection with Modified {
        when(upa.response(CorrelationId(0, 7), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)))))

        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, ReserveFailed(failed))) =>
            failed.getServiceException.getErrorId must_== NsiError.CapacityUnavailable(1000).id
        })
        reservationState must_== ReservationStateEnumType.RESERVE_FAILED
      }
    }

    "in provisioned state" should {
      "have data plane active when data plane state change is received" in new ReservedConnection with Provisioned {
        when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId("ConnectionIdA")
          .withDataPlaneStatus(dataPlaneStatusType(true, consistent = true))
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))

        dataPlaneStatus.isActive() must beTrue
        dataPlaneStatus.isVersionConsistent() must beTrue
        dataPlaneStatus.getVersion() must beEqualTo(ConfirmedCriteriaVersion)

        messages must contain(
          agg.notification(CorrelationId(0, 10), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
            .withConnectionId(ConnectionId)
            .withNotificationId(1)
            .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
            .withDataPlaneStatus(dataPlaneStatusType(true, consistent = true)))))
      }

      "have data plane active when data plane state change is received (multi segement)" in new ReservedConnectionWithTwoSegments with ProvisionedSegments {
        when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId("ConnectionIdA")
          .withDataPlaneStatus(dataPlaneStatusType(true, consistent = true))
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))

        dataPlaneStatus.isActive() must beFalse
        messages must contain(exactly[Message](
          agg.notification(CorrelationId(0, 14), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
            .withConnectionId(ConnectionId)
            .withNotificationId(1)
            .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z"))
            .withDataPlaneStatus(dataPlaneStatusType(false, consistent = false))))))

        when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId("ConnectionIdB")
          .withDataPlaneStatus(dataPlaneStatusType(true, consistent = true))
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:10:00Z")))))

        dataPlaneStatus.isActive() must beTrue
        messages must contain(agg.notification(CorrelationId(0, 16), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(2)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:10:00Z"))
          .withDataPlaneStatus(dataPlaneStatusType(true, consistent = true)))))
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
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, _, LifecycleStateEnumType.TERMINATED, _, _, _) => ok })
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, _, LifecycleStateEnumType.TERMINATING, _, _, _) => ok })
        messages must beEmpty

        when(upa.response(CorrelationId(0, 12), TerminateConfirmed("ConnectionIdB")))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, _, LifecycleStateEnumType.TERMINATED, _, _, _) => ok })
        segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, _, LifecycleStateEnumType.TERMINATED, _, _, _) => ok })
        messages must contain(ToRequester(NsiRequesterMessage(nsiRequesterHeaders(TerminateCorrelationId), TerminateConfirmed(ConnectionId))))
      }

    }

    "be in released state when initial reserve" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
    }

    "initialize the provisioning state machine when the path is confirmed" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

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
      segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, ReservationStateEnumType.RESERVE_START, _, ProvisionStateEnumType.PROVISIONING, _, _) => ok })
      segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, ReservationStateEnumType.RESERVE_START, _, ProvisionStateEnumType.PROVISIONING, _, _) => ok })

      when(upa.response(CorrelationId(0, 11), ProvisionConfirmed("ConnectionIdA")))

      messages must beEmpty
      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONING)
      segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, _, _, ProvisionStateEnumType.PROVISIONED, _, _) => ok })
      segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, _, _, ProvisionStateEnumType.PROVISIONING, _, _) => ok })

      when(upa.response(CorrelationId(0, 12), ProvisionConfirmed("ConnectionIdB")))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONED)
      segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdA"), _, _, _, _, _, _, ProvisionStateEnumType.PROVISIONED, _, _) => ok })
      segments must contain(like[ConnectionData] { case ConnectionData(Present("ConnectionIdB"), _, _, _, _, _, _, ProvisionStateEnumType.PROVISIONED, _, _) => ok })
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
        .withDataPlaneStatus(dataPlaneStatusType(true, consistent = true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")))))

      when(upa.notification(newCorrelationId, DataPlaneStateChange(new DataPlaneStateChangeRequestType()
        .withConnectionId("ConnectionIdA")
        .withDataPlaneStatus(dataPlaneStatusType(false, consistent = true))
        .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:15:00Z")))))

      dataPlaneStatus.isActive() must beFalse
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(
        agg.notification(CorrelationId(0, 12), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(ConnectionId)
          .withNotificationId(2)
          .withTimeStamp(DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:15:00Z"))
          .withDataPlaneStatus(dataPlaneStatusType(false, consistent = true)))))
    }

    "become failed on ForcedEnd error event" in new ReservedConnection {
      val ProviderErrorEventCorrelationId = newCorrelationId
      val TimeStamp = Instant.now().minus(3, ChronoUnit.MINUTES).toXMLGregorianCalendar()
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
      connection.lsm.childConnectionState("ConnectionIdA") must beEqualTo(LifecycleStateEnumType.FAILED)
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
      val endTime = schedule.endTime.fold2(identity, Instant.now, Instant.now)
      context = context.copy(clock = Clock.fixed(endTime, ZoneId.systemDefault))

      when(PassedEndTime(CorrelationId(3, 0), connection.id, endTime))

      lifecycleState must beEqualTo(LifecycleStateEnumType.PASSED_END_TIME)
    }

    "ignore PassedEndTime message before scheduled end time" in new ReservedConnection {
      val endTime = schedule.endTime.fold2(_.minus(5, ChronoUnit.MINUTES), Instant.now, Instant.now)
      context = context.copy(clock = Clock.fixed(endTime, ZoneId.systemDefault))

      when(PassedEndTime(CorrelationId(3, 0), connection.id, endTime))

      lifecycleState must beEqualTo(LifecycleStateEnumType.CREATED)
    }

    "pass MessageDeliveryTimeout notifications to requester" in new ReservedConnection {
      val TimedOutMessageCorrelationId = newCorrelationId.toString
      val TimeStamp = Instant.now().minus(3, ChronoUnit.MINUTES).toXMLGregorianCalendar()

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
      val TimeStamp = Instant.now().minus(3, ChronoUnit.MINUTES).toXMLGregorianCalendar()

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
      val TimeStamp = Instant.now().minus(3, ChronoUnit.MINUTES).toXMLGregorianCalendar()
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
      when(ura.request(newCorrelationId, QueryRecursive(Some(Left(ConnectionId :: Nil)), None)))

      messages must haveSize(2)
      messages must contain(like[Message] {
        case ToProvider(NsiProviderMessage(_, QueryRecursive(Some(Left("ConnectionIdA" :: Nil)), None)), provider) => provider must_== A.provider
        case ToProvider(NsiProviderMessage(_, QueryRecursive(Some(Left("ConnectionIdB" :: Nil)), None)), provider) => provider must_== B.provider
      }).forall
    }

    "send query recursive confirm to requester when all child providers replied" in new ReservedConnectionWithTwoSegments {
      given(
        ura.request(newCorrelationId, QueryRecursive(Some(Left(ConnectionId :: Nil)), None)),
        upa.response(CorrelationId(0, 11), QueryRecursiveConfirmed(Nil)))

      when(upa.response(CorrelationId(0, 12), QueryRecursiveConfirmed(Nil)))

      messages must haveSize(1)
      messages must contain(like[Message] {
        case ToRequester(NsiRequesterMessage(_, QueryRecursiveConfirmed(results))) => results must haveSize(1)
      }).forall
    }
  }
}
