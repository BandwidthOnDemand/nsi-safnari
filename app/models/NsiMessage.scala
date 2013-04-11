package models

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
import support._
import javax.xml.bind.ValidationEventHandler
import javax.xml.bind.ValidationEvent

case class NsiHeaders(correlationId: UUID, replyTo: Option[URI]) {
  def asDocument: Document = {
    val factory = new ObjectFactory()
    val header = factory.createCommonHeaderType()
    header.setCorrelationId(f"urn:uuid:${correlationId}")
    header.setReplyTo(replyTo.map(_.toASCIIString()).orNull)
    header.setProtocolVersion("1")
    header.setProviderNSA("ProviderNSA")
    header.setRequesterNSA("RequesterNSA")

    NsiMessage.marshal(factory.createNsiHeader(header))
  }
}

trait NsiMessage {
  def headers: NsiHeaders
  def correlationId = headers.correlationId
  def replyTo = headers.replyTo

  def bodyDocument: Document

  def headerDocument = headers.asDocument
}
object NsiMessage {
  private def newDocument = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()

  private val schema = {
    val schemas = Array("wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd", "ogf_nsi_connection_interface_types_v2_0.xsd")
    val sources = schemas.map(Thread.currentThread().getContextClassLoader().getResource).map(schema => new StreamSource(schema.toExternalForm()): Source)
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.newSchema(sources)
  }
  private val context = JAXBContext.newInstance("org.ogf.schemas.nsi._2013._04.framework.types:org.ogf.schemas.nsi._2013._04.connection.types:org.ogf.schemas.nsi._2013._04.connection._interface:org.ogf.schemas.nsi._2013._04.framework.headers")

  def marshaller = context.createMarshaller().tap(_.setSchema(schema))
  def unmarshaller = context.createUnmarshaller().tap(_.setSchema(schema)).tap(_.setEventHandler(new ValidationEventHandler {
    def handleEvent(event: ValidationEvent) = event.getMessage().contains("cvc-complex-type.2.3") && event.getMessage().contains("cannot have character [children], because the type's content type is element-only")
  }))

  def marshal(jaxbElement: AnyRef): Document = newDocument.tap(marshaller.marshal(jaxbElement, _))
}
