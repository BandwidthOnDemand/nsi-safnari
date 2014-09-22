package presenters

import org.ogf.schemas.nsi._2013._12.connection.types._

class ConnectionPresenterTest extends helpers.Specification {
  "A connection" >> {
    def states = new ConnectionStatesType()
      .withReservationState(ReservationStateEnumType.RESERVE_HELD)
      .withProvisionState(ProvisionStateEnumType.PROVISIONED)
      .withLifecycleState(LifecycleStateEnumType.CREATED)
    def data = new QuerySummaryResultType().withConnectionId("ID").withGlobalReservationId("GLOBAL").withDescription("description")
      .withRequesterNSA("requester").withConnectionStates(states)

    "given a query summary result" should {
      val subject = ConnectionPresenter(data)

      "have a connection ID" in {
        subject.connectionId must beEqualTo(data.getConnectionId)
      }

      "have a description" in {
        subject.description must beEqualTo(data.getDescription)
      }

      "have a global reservation ID" in {
        subject.globalReservationId must beEqualTo(data.getGlobalReservationId)
      }

      "have a requester NSA" in {
        subject.requesterNsa must beEqualTo(data.getRequesterNSA)
      }

      "have a reservation state" in {
        subject.reservationState must beEqualTo(data.getConnectionStates.getReservationState.value)
      }

      "have a provision state" in {
        subject.provisionState must beEqualTo(data.getConnectionStates.getProvisionState.value)
      }

      "have a lifecycle state" in {
        subject.lifecycleState must beEqualTo(data.getConnectionStates.getLifecycleState.value)
      }
    }

    "given an active connection" should {
      val subject = ConnectionPresenter(data.withConnectionStates(states.withDataPlaneStatus( new DataPlaneStatusType().withActive(true) )))

      "have an active data plane" in {
        subject.dataPlaneStatus must beEqualTo("active")
      }
    }

    "given an inactive connection" should {
      val subject = ConnectionPresenter(data.withConnectionStates(states.withDataPlaneStatus( new DataPlaneStatusType().withActive(false) )))

      "have an inactive data plane" in {
        subject.dataPlaneStatus must beEqualTo("inactive")
      }
    }
  }
}