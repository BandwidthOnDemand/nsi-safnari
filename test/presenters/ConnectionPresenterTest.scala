package presenters

import nl.surfnet.safnari._

import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types._

class ConnectionPresenterTest extends helpers.Specification {
  "A connection" >> {
    def states = new ConnectionStatesType()
      .withReservationState(ReservationStateEnumType.RESERVE_HELD)
      .withProvisionState(ProvisionStateEnumType.PROVISIONED)
      .withLifecycleState(LifecycleStateEnumType.CREATED)
      .withDataPlaneStatus(new DataPlaneStatusType().withActive(false))
    def data = new QuerySummaryResultType().withConnectionId("ID").withGlobalReservationId("GLOBAL").withDescription("description")
      .withRequesterNSA("requester").withConnectionStates(states)
    def criteria = new ReservationConfirmCriteriaType().withSchedule(new ScheduleType)

    val now = DateTime.now

    "given a query summary result" should {
      val subject = ConnectionPresenter(data, criteria)

      "have a connection ID" in {
        subject.connectionId must beEqualTo(data.getConnectionId)
      }

      "have a description" in {
        subject.description must beEqualTo(data.getDescription)
      }

      "have a global reservation ID" in {
        subject.globalReservationId must beEqualTo(Some(data.getGlobalReservationId))
      }

      "have a requester NSA" in {
        subject.requesterNsa must beEqualTo(data.getRequesterNSA)
      }

      "have a status" in {
        subject.status must not(beEmpty)
      }
    }

    "given an active connection" should {
      val schedule = criteria.getSchedule.withStartTime(now.minusDays(1).toXmlGregorianCalendar).withEndTime(now.plusDays(1).toXmlGregorianCalendar)
      val subject = ConnectionPresenter(data.withConnectionStates(states.withDataPlaneStatus( new DataPlaneStatusType().withActive(true) )),
                                        criteria.withSchedule(schedule))

      "have an active data plane" in {
        subject.dataPlaneStatus must beEqualTo("active")
      }

      "qualify as 'active" in {
        subject.qualifier(now) must beEqualTo('active)
      }
    }

    "given a future connection" should {
      val schedule = criteria.getSchedule.withStartTime(now.plusDays(1).toXmlGregorianCalendar).withEndTime(now.plusDays(5).toXmlGregorianCalendar)
      val subject = ConnectionPresenter(data.withConnectionStates(states.withDataPlaneStatus( new DataPlaneStatusType().withActive(false) )),
                                        criteria.withSchedule(schedule))

      "have an inactive data plane" in {
        subject.dataPlaneStatus must beEqualTo("inactive")
      }

      "qualify as 'future" in {
        subject.qualifier(now) must beEqualTo('future)
      }
    }

    "given a past connection" should {
      val schedule = criteria.getSchedule.withStartTime(now.minusDays(5).toXmlGregorianCalendar).withEndTime(now.minusDays(1).toXmlGregorianCalendar)
      val subject = ConnectionPresenter(data.withConnectionStates(states.withDataPlaneStatus( new DataPlaneStatusType().withActive(false) )),
                                        criteria.withSchedule(schedule))

      "have an inactive data plane" in {
        subject.dataPlaneStatus must beEqualTo("inactive")
      }

      "qualify as 'past" in {
        subject.qualifier(now) must beEqualTo('past)
      }
    }
  }
}
