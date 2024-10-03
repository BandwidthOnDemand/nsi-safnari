package presenters

import java.time.Instant
import java.time.temporal.*
import nl.surfnet.nsiv2.utils.*
import org.ogf.schemas.nsi._2013._12.connection.types.*

class ConnectionPresenterTest extends helpers.Specification {
  "A connection" >> {
    def states = new ConnectionStatesType()
      .withReservationState(ReservationStateEnumType.RESERVE_HELD)
      .withProvisionState(ProvisionStateEnumType.PROVISIONED)
      .withLifecycleState(LifecycleStateEnumType.CREATED)
      .withDataPlaneStatus(new DataPlaneStatusType().withActive(false))
    def data = new QuerySummaryResultType()
      .withConnectionId("ID")
      .withGlobalReservationId("GLOBAL")
      .withDescription("description")
      .withRequesterNSA("requester")
      .withConnectionStates(states)
    def criteria = new ReservationRequestCriteriaType().withSchedule(new ScheduleType)

    val now = Instant.now

    "given a query summary result" should {
      val subject = ConnectionPresenter(data, Some(criteria))

      "have a connection ID" in {
        subject.connectionId must beEqualTo(data.getConnectionId)
      }

      "have a description" in {
        subject.description must beSome(data.getDescription)
      }

      "have a global reservation ID" in {
        subject.globalReservationId must beEqualTo(Some(data.getGlobalReservationId))
      }

      "have a requester NSA" in {
        subject.requesterNsa must beEqualTo(data.getRequesterNSA)
      }

      "have a status" in {
        subject.status must not(beEmpty[String])
      }
    }

    "given an active connection" should {
      val schedule = criteria.getSchedule
        .withStartTime(now.minus(1, ChronoUnit.DAYS).toXMLGregorianCalendar())
        .withEndTime(now.plus(1, ChronoUnit.DAYS).toXMLGregorianCalendar())
      val subject = ConnectionPresenter(
        data.withConnectionStates(
          states.withDataPlaneStatus(new DataPlaneStatusType().withActive(true))
        ),
        Some(criteria.withSchedule(schedule))
      )

      "have an active data plane" in {
        subject.dataPlaneStatus must beEqualTo("active")
      }

      "qualify as 'current" in {
        subject.qualifier(now) must beEqualTo("current")
      }
    }

    "given a future connection" should {
      val schedule = criteria.getSchedule
        .withStartTime(now.plus(1, ChronoUnit.DAYS).toXMLGregorianCalendar())
        .withEndTime(now.plus(5, ChronoUnit.DAYS).toXMLGregorianCalendar())
      val subject = ConnectionPresenter(
        data.withConnectionStates(
          states.withDataPlaneStatus(new DataPlaneStatusType().withActive(false))
        ),
        Some(criteria.withSchedule(schedule))
      )

      "have an inactive data plane" in {
        subject.dataPlaneStatus must beEqualTo("inactive")
      }

      "qualify as 'future" in {
        subject.qualifier(now) must beEqualTo("future")
      }
    }

    "given a past connection" should {
      val schedule = criteria.getSchedule
        .withStartTime(now.minus(5, ChronoUnit.DAYS).toXMLGregorianCalendar())
        .withEndTime(now.minus(1, ChronoUnit.DAYS).toXMLGregorianCalendar())
      val subject = ConnectionPresenter(
        data.withConnectionStates(
          states.withDataPlaneStatus(new DataPlaneStatusType().withActive(false))
        ),
        Some(criteria.withSchedule(schedule))
      )

      "have an inactive data plane" in {
        subject.dataPlaneStatus must beEqualTo("inactive")
      }

      "qualify as 'past" in {
        subject.qualifier(now) must beEqualTo("past")
      }
    }
  }
}
