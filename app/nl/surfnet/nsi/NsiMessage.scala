package nl.surfnet.nsi

import java.net.URI
import java.util.UUID
import javax.xml.XMLConstants
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import org.ogf.schemas.nsi._2013._04.framework.headers.ObjectFactory
import org.w3c.dom.Document

trait Request {
  def correlationId: CorrelationId
}
trait Response {
  def correlationId: CorrelationId
}

case class NsiHeaders(correlationId: UUID, replyTo: Option[URI]) {
  def asReply: NsiHeaders = copy(replyTo = None)

  def asDocument: Document = {
    val factory = new ObjectFactory()
    val header = factory.createCommonHeaderType()
    header.setCorrelationId(f"urn:uuid:${correlationId}")
    header.setReplyTo(replyTo.map(_.toASCIIString()).orNull)
    header.setProtocolVersion("2.0")
    header.setProviderNSA("ProviderNSA")
    header.setRequesterNSA("RequesterNSA")

    NsiMessage.marshal(factory.createNsiHeader(header))
  }
}

trait NsiMessage {
  def headers: NsiHeaders
  def correlationId = headers.correlationId
  def replyTo = headers.replyTo
  def optionalConnectionId: Option[ConnectionId]

  def bodyDocument: Document

  def headerDocument = headers.asDocument
}
object NsiMessage {
  private def newDocument = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()

  private val SchemaLocations = Seq("wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd", "ogf_nsi_connection_interface_types_v2_0.xsd")
  private val SchemaPackages = Seq("org.ogf.schemas.nsi._2013._04.framework.headers", "org.ogf.schemas.nsi._2013._04.connection.types", "org.ogf.schemas.nsi._2013._04.connection._interface")

  private val nsiSchema = {
    val schemaSources = SchemaLocations.map(location => new StreamSource(classpathResourceUri(location).toASCIIString()))

    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.newSchema(schemaSources.toArray[Source])
  }
  private val jaxbContext = JAXBContext.newInstance(SchemaPackages.mkString(":"))

  def marshaller = jaxbContext.createMarshaller().tap(_.setSchema(nsiSchema))
  def unmarshaller = jaxbContext.createUnmarshaller().tap(_.setSchema(nsiSchema))

  /* To allow whitespace in between elements set the following event handler on the unmarshaller:

     new ValidationEventHandler {
       def handleEvent(event: ValidationEvent) = event.getMessage().contains("cvc-complex-type.2.3") && event.getMessage().contains("cannot have character [children], because the type's content type is element-only")
     }
   */

  def marshal(jaxbElement: AnyRef): Document = newDocument.tap(document => marshaller.marshal(jaxbElement, document))
}
