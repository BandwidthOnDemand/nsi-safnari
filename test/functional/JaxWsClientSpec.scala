package functional

import org.junit.runner.RunWith
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import java.net.URL
import java.util.Collections
import org.ogf.schemas.nsi._2013._12.connection.provider.ConnectionServiceProvider
import org.ogf.schemas.nsi._2013._12.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._12.connection.requester.ConnectionServiceRequester
import javax.xml.ws.Holder

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class JaxWsClientSpec extends helpers.Specification {
  sequential

  val ServerPort = Helpers.testServerPort
  val SafnariNsa = "urn:ogf:network:nsa:surfnet-nsi-safnari"
  def Application = new GuiceApplicationBuilder().configure(Map("nsi.base.url" -> s"http://localhost:$ServerPort", "safnari.nsa.id" -> SafnariNsa)).build

  "A JAX WS client" should {

    "be able to talk to the connection provider endpoint" in new WithServer(Application, ServerPort) {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      val header = new Holder(new CommonHeaderType()
        .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
        .withProtocolVersion("2")
        .withRequesterNSA("urn:ogf:network:surfnet-fake-requester")
        .withProviderNSA(SafnariNsa))

      service.getConnectionServiceProviderPort().querySummary(Collections.emptyList[String], Collections.emptyList[String], null, header)
    }

    "be able to talk to the connection requester endpoint" in new WithServer(Application, ServerPort) {
      val service = new ConnectionServiceRequester(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceRequester"))

      val header = new Holder(new CommonHeaderType()
        .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
        .withProtocolVersion("2")
        .withRequesterNSA(SafnariNsa)
        .withProviderNSA("urn:ogf:network:surfnet-fake-provider"))

      service.getConnectionServiceRequesterPort().reserveCommitConfirmed("123-abc", header)
    }
  }

}
