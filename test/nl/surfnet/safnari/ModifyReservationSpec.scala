package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.services.types.TypeValueType

import nl.surfnet.bod.nsi.Nillable
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._
import helpers.NsiMessages._


@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ModifyReservationSpec extends helpers.ConnectionEntitySpecification {
  "A connection" >> {
    "that has been committed" should {
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
        connection.query.getCriteria.get(0).pointToPointService.map(_.getCapacity) must beSome(500L)
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
            failed.getServiceException.getErrorId must_== NsiError.ChildSegmentError.id
        })
        reservationState must_== ReservationStateEnumType.RESERVE_FAILED
      }

      "become failed when modify is not supported by child" in new ReservedConnection with Modified {
        when(upa.acknowledge(CorrelationId(0, 7), ServiceException(NsiError.NotImplemented.toServiceException(A.provider.nsa).withConnectionId("ConnectionIdA"))))

        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, ReserveFailed(failed))) =>
            failed.getServiceException.getErrorId must_== NsiError.ChildSegmentError.id
        })
        reservationState must_== ReservationStateEnumType.RESERVE_FAILED
      }

      "not send ReserveAbort to child that does not support modify" in new ReservedConnection with Modified {
        given(upa.acknowledge(CorrelationId(0, 7), ServiceException(NsiError.NotImplemented.toServiceException(A.provider.nsa).withConnectionId("ConnectionIdA"))))

        val AbortModifyCorrelationId = newCorrelationId
        when(ura.request(AbortModifyCorrelationId, ReserveAbort(connection.id)))

        messages must contain(like[Message] {
          case ToRequester(NsiRequesterMessage(_, ReserveAbortConfirmed(_))) =>
            ok
        })
        reservationState must_== ReservationStateEnumType.RESERVE_START
      }
    }
  }
}
