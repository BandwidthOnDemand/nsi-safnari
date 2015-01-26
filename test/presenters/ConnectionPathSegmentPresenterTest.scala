package presenters

import nl.surfnet.safnari.ConnectionData
import org.ogf.schemas.nsi._2013._12.connection.types.{DataPlaneStatusType, ProvisionStateEnumType, LifecycleStateEnumType, ReservationStateEnumType}
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionPathSegmentPresenterTest extends helpers.Specification {
  "A ConnectionPathSegmentPresenter" >> {
    val data = ConnectionData(Some("ID"), "urn:ogf:network:surfnet.nl:1900:provider",
      "urn:ogf:network:surfnet.nl:1900:source",
      "urn:ogf:network:surfnet.nl:1900:destination",
      ReservationStateEnumType.RESERVE_START, LifecycleStateEnumType.CREATED, ProvisionStateEnumType.PROVISIONED, new DataPlaneStatusType().withVersion(1).withActive(true).withVersionConsistent(true),
      Some(new ServiceExceptionType())
    )

    "given an active connection" should {
      val subject = ConnectionPathSegmentPresenter(data.copy(dataPlaneStatus = new DataPlaneStatusType().withVersion(1).withActive(true).withVersionConsistent(true)))

      "have a connectionId" in {
        subject.connectionId must beEqualTo(data.connectionId)
      }

      "have a provider NSA" in {
        subject.providerNsa must beEqualTo("urn:ogf:network:surfnet.nl:1900:provider")
        subject.providerNsaShort must beEqualTo("surfnet.nl:1900:provider")
      }

      "have a status" in {
        subject.status must not(beEmpty)
      }

      "have an active data plane status" in {
        subject.dataPlaneStatus must beEqualTo("Active")
      }

      "have a last service exception" in {
        subject.lastServiceException must beEqualTo(data.lastServiceException)
      }

      "have a source and destination" in {
        subject.source must beEqualTo("urn:ogf:network:surfnet.nl:1900:source")
        subject.sourceShort must beEqualTo("surfnet.nl:1900:source")
        subject.destination must beEqualTo("urn:ogf:network:surfnet.nl:1900:destination")
        subject.destinationShort must beEqualTo("surfnet.nl:1900:destination")
      }
    }

    "given an inactive connection" should {
      val subject = ConnectionPathSegmentPresenter(data.copy(dataPlaneStatus = new DataPlaneStatusType().withVersion(1).withActive(false).withVersionConsistent(true)))

      "have an inactive data plane status" in {
        subject.dataPlaneStatus must beEqualTo("Inactive")
      }
    }
  }
}