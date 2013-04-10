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

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class JaxWsClientSpec extends Specification {

  "A JAX WS client" should {

    "be able to talk to the connection provider endpoint" in new WithServer {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      service.setHandlerResolver(new HandlerResolver() {
        override def getHandlerChain(portInfo: PortInfo) = {
          List[Handler[_ <: MessageContext]](new NsiHeaderHandler).asJava
        }
      })

      service.getConnectionServiceProviderPort().querySummary(Collections.emptyList[String], Collections.emptyList[String])
    }

    "be able to talk to the connection requester endpoint" in new WithServer {
      val service = new ConnectionServiceRequester(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceRequester"))

      service.getConnectionServiceRequesterPort().reserveCommitConfirmed("", "123-abc")
    }
  }

  class NsiHeaderHandler extends SOAPHandler[SOAPMessageContext] {

    def handleMessage(messageContext: SOAPMessageContext): Boolean = {
      if (isOutboundMessage(messageContext)) {

        val doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()
        JAXBContext.newInstance(classOf[CommonHeaderType]).createMarshaller().marshal(createNsiHeader(), doc)

        val soapHeader = messageContext.getMessage().getSOAPPart().getEnvelope().addHeader()

        soapHeader.addChildElement(SOAPFactory.newInstance().createElement(doc.getDocumentElement()))
      }

      true
    }

    private def createNsiHeader() = {
        val factory = new HeadersObjectFactory()
        val header = factory.createCommonHeaderType()
        header.setCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
        header.setProtocolVersion("0")
        header.setRequesterNSA("")
        header.setProviderNSA("")
        factory.createNsiHeader(header)
    }

    private def isOutboundMessage(messageContext: MessageContext): Boolean =
      messageContext.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY).asInstanceOf[Boolean];

    def close(x$1: javax.xml.ws.handler.MessageContext): Unit = ()
    def handleFault(x$1: javax.xml.ws.handler.soap.SOAPMessageContext): Boolean = true
    def getHeaders(): java.util.Set[javax.xml.namespace.QName] = Collections.emptySet[javax.xml.namespace.QName]
  }
}