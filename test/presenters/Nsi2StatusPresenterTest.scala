package presenters

import org.ogf.schemas.nsi._2013._12.connection.types.{ProvisionStateEnumType, ReservationStateEnumType, LifecycleStateEnumType}

class Nsi2StatusPresenterTest extends helpers.Specification {
  "Nsi2StatusPresenter" >> {
    "given a terminated connection state" should {
      val subject = Nsi2StatusPresenter(LifecycleStateEnumType.TERMINATED,
                                        ReservationStateEnumType.RESERVE_START,
                                        ProvisionStateEnumType.RELEASED,
                                        dataPlaneActive = false)
      "have no status" in {
        subject.status must beEqualTo("Terminated")
      }
    }

    "given an active connection state" should {
      val subject = Nsi2StatusPresenter(LifecycleStateEnumType.CREATED,
        ReservationStateEnumType.RESERVE_START,
        ProvisionStateEnumType.PROVISIONED,
        dataPlaneActive = true)
      "have a status that shows provision state and data plane" in {
        subject.status must beEqualTo("Provisioned, Active")
      }
    }

    "given a released connection state" should {
      val subject = Nsi2StatusPresenter(LifecycleStateEnumType.CREATED,
        ReservationStateEnumType.RESERVE_START,
        ProvisionStateEnumType.RELEASED,
        dataPlaneActive = false)
      "have a status that shows provision state and data plane" in {
        subject.status must beEqualTo("Released, Inactive")
      }
    }

    "given a failed connection state" should {
      val subject = Nsi2StatusPresenter(LifecycleStateEnumType.FAILED,
        ReservationStateEnumType.RESERVE_FAILED,
        provision = null,
        dataPlaneActive = false)
      "have a status that shows lifecycle and reserve states" in {
        subject.status must beEqualTo("ReserveFailed, Failed")
      }
    }

    "given a reserve held state" should {
      val subject = Nsi2StatusPresenter(LifecycleStateEnumType.CREATED,
        ReservationStateEnumType.RESERVE_HELD,
        provision = null,
        dataPlaneActive = false)
      "have a status that shows reserve state" in {
        subject.status must beEqualTo("ReserveHeld")
      }
    }
  }
}
