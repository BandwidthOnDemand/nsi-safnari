package functional

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import play.api.test._
import org.ogf.schemas.nsi._2013._04.connection.provider.ConnectionProviderPort
import org.ogf.schemas.nsi._2013._04.connection.provider.ConnectionServiceProvider
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
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._04.framework.headers.{ObjectFactory => HeadersObjectFactory}
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import org.ogf.schemas.nsi._2013._04.connection.requester.ConnectionServiceRequester
import com.sun.xml.internal.ws.client.ClientTransportException
import nl.surfnet.nsi.NsiMessage
import nl.surfnet.nsi.NsiHeaders
import java.util.UUID
import org.specs2.execute.PendingUntilFixed
import javax.xml.ws.Holder

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class JaxWsClientSpec extends Specification with PendingUntilFixed {

  "A JAX WS client" should {

    val NsiHeader = new Holder(new CommonHeaderType()
      .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
      .withProtocolVersion("2")
      .withRequesterNSA("urn:ogf:network:surfnet")
      .withProviderNSA("urn:ogf:network:safnari"))

    "be able to talk to the connection provider endpoint" in new WithServer {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      service.getConnectionServiceProviderPort().querySummary(Collections.emptyList[String], Collections.emptyList[String], NsiHeader)
    }

    "be able to talk to the connection requester endpoint" in new WithServer {
      val service = new ConnectionServiceRequester(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceRequester"))

      val header = new Holder(new CommonHeaderType()
        .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
        .withProtocolVersion("2")
        .withRequesterNSA("urn:ogf:network:surfnet")
        .withProviderNSA("urn:ogf:network:safnari"))

      service.getConnectionServiceRequesterPort().reserveCommitConfirmed("", "123-abc", NsiHeader)
    }
  }

}