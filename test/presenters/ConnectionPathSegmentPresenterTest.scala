package presenters

import nl.surfnet.safnari.ConnectionData
import org.ogf.schemas.nsi._2013._12.connection.types.{ProvisionStateEnumType, LifecycleStateEnumType, ReservationStateEnumType}
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionPathSegmentPresenterTest extends helpers.Specification {
  "A ConnectionPathSegmentPresenter" >> {
    val data = ConnectionData(Some("ID"), "provider",
      ReservationStateEnumType.RESERVE_START, LifecycleStateEnumType.CREATED, ProvisionStateEnumType.PROVISIONED, true,
      Some(new ServiceExceptionType())
    )

    "given an active connection" should {
      val subject = ConnectionPathSegmentPresenter(data.copy(dataPlaneStatus = true))

      "have a connectionId" in {
        subject.connectionId must beEqualTo(data.connectionId)
      }

      "have a provider NSA" in {
        subject.providerNsa must beEqualTo(data.providerNsa)
      }

      "have a status" in {
        subject.status must not(beEmpty)
      }

      "have an active data plane status" in {
        subject.dataPlaneStatus must beEqualTo("active")
      }

      "have a last service exception" in {
        subject.lastServiceException must beEqualTo(data.lastServiceException)
      }
    }

    "given an inactive connection" should {
      val subject = ConnectionPathSegmentPresenter(data.copy(dataPlaneStatus = false))

      "have an inactive data plane status" in {
        subject.dataPlaneStatus must beEqualTo("inactive")
      }
    }
  }
}