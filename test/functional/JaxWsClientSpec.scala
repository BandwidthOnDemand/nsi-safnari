package functional

import org.junit.runner.RunWith
import play.api.test._
import java.net.URL
import java.util.Collection
import java.util.Collections
import scala.collection.JavaConverters._
import javax.xml.ws.handler.{HandlerResolver, Handler}
import javax.xml.ws.handler.soap.SOAPMessageContext
import javax.xml.ws.handler.PortInfo
import javax.xml.ws.handler.soap.SOAPHandler
import javax.xml.ws.handler.MessageContext
import java.util.ArrayList
import javax.xml.soap.SOAPFactory
import org.ogf.schemas.nsi._2013._12.connection.provider.ConnectionProviderPort
import org.ogf.schemas.nsi._2013._12.connection.provider.ConnectionServiceProvider
import org.ogf.schemas.nsi._2013._12.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._12.framework.headers.{ObjectFactory => HeadersObjectFactory}
import org.ogf.schemas.nsi._2013._12.connection.requester.ConnectionServiceRequester
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import com.sun.xml.internal.ws.client.ClientTransportException
import nl.surfnet.safnari.NsiMessage
import nl.surfnet.safnari.NsiHeaders
import java.util.UUID
import javax.xml.ws.Holder

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class JaxWsClientSpec extends helpers.Specification {
  sequential

  val ServerPort = Helpers.testServerPort
  val SafnariNsa = "urn:ogf:network:nsa:surfnet-nsi-safnari"
  def Application = FakeApplication(additionalConfiguration = Map("nsi.base.url" -> s"http://localhost:$ServerPort", "safnari.nsa" -> SafnariNsa))

  "A JAX WS client" should {

    "be able to talk to the connection provider endpoint" in new WithServer(Application, ServerPort) {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      val header = new Holder(new CommonHeaderType()
        .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
        .withProtocolVersion("2")
        .withRequesterNSA("urn:ogf:network:surfnet-fake-requester")
        .withProviderNSA(SafnariNsa))

      service.getConnectionServiceProviderPort().querySummary(Collections.emptyList[String], Collections.emptyList[String], header)
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
