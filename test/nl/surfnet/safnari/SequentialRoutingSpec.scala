package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType

import scala.jdk.CollectionConverters._

import nl.surfnet.nsiv2.messages._
import helpers.NsiMessages._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class SequentialRoutingSpec extends helpers.ConnectionEntitySpecification {
  "Connection Entity" >> {
    "with sequential routing" should {
      abstract class SequentialRoutingFixture extends fixture {
        override def pathComputationAlgorithm = PathComputationAlgorithm.Sequential
      }

      "confirm the reservation with a single path segment with sequential routing" in new SequentialRoutingFixture {
        val ConfirmCriteriaWithQualifiedStps = ConfirmCriteria.withPointToPointService(
          Service.withSourceSTP("networkId:A?vlan=1").withDestSTP("networkId:B?vlan=2")
        )

        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A),
          upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA"))
        )

        when(
          upa.response(
            CorrelationId(0, 4),
            ReserveConfirmed("ConnectionIdA", ConfirmCriteriaWithQualifiedStps)
          )
        )

        messages must contain(
          agg.response(
            ReserveCorrelationId,
            ReserveConfirmed(ConnectionId, ConfirmCriteriaWithQualifiedStps),
            any = pathTrace(
              AggregatorNsa,
              ConnectionId,
              (A.provider.nsa, "ConnectionIdA") -> Nil
            ) :: Nil
          )
        )

        childConnectionData("ConnectionIdA").sourceStp must beEqualTo("networkId:A?vlan=1")
        childConnectionData("ConnectionIdA").destinationStp must beEqualTo("networkId:B?vlan=2")
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
      }

      "be in reservation held state when both segments are confirmed" in new SequentialRoutingFixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 1), A, B),
          A.provider.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA"))
        )

        segments must contain(like[ConnectionData] {
          case ConnectionData(
                Present("ConnectionIdA"),
                _,
                _,
                _,
                _,
                ReservationStateEnumType.RESERVE_CHECKING,
                _,
                _,
                _,
                _
              ) =>
            ok
        })

        when(
          A.provider.response(
            CorrelationId(0, 4),
            ReserveConfirmed(
              "ConnectionIdA",
              ConfirmCriteria.withPointToPointService(
                A.serviceType.service
                  .withSourceSTP("networkId:A:A?vlan=3")
                  .withDestSTP("networkId:A:B?vlan=99")
              )
            )
          )
        )

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
        messages must contain(beLike[Message] {
          case ToProvider(NsiProviderMessage(_, reserve @ InitialReserve(_)), provider)
              if provider == B.provider =>
            reserve.service must beSome.which((x: P2PServiceBaseType) =>
              x.getSourceSTP() must_== "X?vlan=99"
            )
        })

        when(B.provider.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")))

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)
        segments must contain(like[ConnectionData] {
          case ConnectionData(
                Present("ConnectionIdA"),
                _,
                _,
                _,
                _,
                ReservationStateEnumType.RESERVE_HELD,
                _,
                _,
                _,
                _
              ) =>
            ok
        })
        segments must contain(like[ConnectionData] {
          case ConnectionData(
                Present("ConnectionIdB"),
                _,
                _,
                _,
                _,
                ReservationStateEnumType.RESERVE_CHECKING,
                _,
                _,
                _,
                _
              ) =>
            ok
        })

        when(
          B.provider.response(
            CorrelationId(0, 5),
            ReserveConfirmed(
              "ConnectionIdB",
              ConfirmCriteria.withPointToPointService(
                A.serviceType.service
                  .withSourceSTP("networkId:B:A?vlan=99")
                  .withDestSTP("networkId:B:B?vlan=23")
              )
            )
          )
        )

        messages must contain(
          agg.response(
            ReserveCorrelationId,
            ReserveConfirmed(
              ConnectionId,
              ConfirmCriteria.withPointToPointService(
                A.serviceType.service
                  .withSourceSTP("networkId:A:A?vlan=3")
                  .withDestSTP("networkId:B:B?vlan=23")
              )
            ),
            any = pathTrace(
              AggregatorNsa,
              ConnectionId,
              (A.provider.nsa, "ConnectionIdA") -> Nil,
              (B.provider.nsa, "ConnectionIdB") -> Nil
            ) :: Nil
          )
        )

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
        segments must contain(like[ConnectionData] {
          case ConnectionData(
                Present("ConnectionIdA"),
                _,
                _,
                _,
                _,
                ReservationStateEnumType.RESERVE_HELD,
                _,
                _,
                _,
                _
              ) =>
            ok
        })
        segments must contain(like[ConnectionData] {
          case ConnectionData(
                Present("ConnectionIdB"),
                _,
                _,
                _,
                _,
                ReservationStateEnumType.RESERVE_HELD,
                _,
                _,
                _,
                _
              ) =>
            ok
        })
      }

      "immediately fail the reservation with two segments when the first one fails" in new SequentialRoutingFixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B)
        )

        when(
          upa.response(
            CorrelationId(0, 4),
            ReserveFailed(
              new GenericFailedType()
                .withConnectionId("ConnectionIdA")
                .withServiceException(
                  NsiError.CapacityUnavailable(1000).toServiceException(A.provider.nsa)
                )
            )
          )
        )

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
            headers must beEqualTo(nsiRequesterHeaders(ReserveCorrelationId))
            failed.getConnectionId() must beEqualTo(ConnectionId)
            failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildSegmentError.id)
            failed.getServiceException().getChildException().asScala must haveSize(1)
        })
        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
      }

      "duplicate initial reserve request should resend reserve request for current segment" in new SequentialRoutingFixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B)
        )

        when(
          A.provider.response(
            CorrelationId(0, 4),
            ReserveConfirmed(
              "ConnectionIdA",
              ConfirmCriteria.withPointToPointService(
                A.serviceType.service
                  .withSourceSTP("networkId:A:A?vlan=3")
                  .withDestSTP("networkId:A:B?vlan=99")
              )
            )
          )
        )

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToProvider(NsiProviderMessage(_, _: InitialReserve), _) => ok
        })
        val reserveB = messages.head

        when(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        messages must haveSize(1)
        messages must contain(reserveB)
      }

      "duplicate initial reserve request should resend reserve confirm reply" in new SequentialRoutingFixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B),
          A.provider.response(
            CorrelationId(0, 4),
            ReserveConfirmed(
              "ConnectionIdA",
              ConfirmCriteria.withPointToPointService(
                A.serviceType.service
                  .withSourceSTP("networkId:A:A?vlan=3")
                  .withDestSTP("networkId:A:B?vlan=99")
              )
            )
          )
        )

        when(
          B.provider.response(
            CorrelationId(0, 5),
            ReserveConfirmed(
              "ConnectionIdB",
              ConfirmCriteria.withPointToPointService(
                A.serviceType.service
                  .withSourceSTP("networkId:B:A?vlan=99")
                  .withDestSTP("networkId:B:B?vlan=23")
              )
            )
          )
        )

        messages must haveSize(1)
        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, _: ReserveConfirmed)) => ok
        })
        val reserveConfirmed = messages.head

        when(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

        messages must haveSize(1)
        messages must contain(reserveConfirmed)
      }

      "terminate the reservation when downstream initial reserve communication fails" in new SequentialRoutingFixture {
        given(
          ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
          pce.confirm(CorrelationId(0, 3), A, B)
        )

        when(
          upa.error(
            CorrelationId(0, 4),
            new ServiceExceptionType()
              .withNsaId("ConnectionIdA")
              .withErrorId("ErrorId")
              .withText("communication error")
          )
        )

        reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)

        when(ura.request(CorrelationId(1, 0), Terminate(ConnectionId)))

        lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
      }
    }
  }
}
