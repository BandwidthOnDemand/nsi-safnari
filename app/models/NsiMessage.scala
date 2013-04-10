package models

import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.framework.headers.ObjectFactory
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.bind.JAXBContext
import org.ogf.schemas.nsi._2013._04._

trait NsiMessage {
  def headers: NsiHeaders
  def correlationId = headers.correlationId
  def replyTo = headers.replyTo

  def bodyDocument: Document

  def headerDocument = {
    val factory = new ObjectFactory()
    val header = factory.createCommonHeaderType()
    header.setCorrelationId(headers.correlationId)
    header.setReplyTo(headers.replyTo.orNull)
    header.setProtocolVersion("1")
    header.setProviderNSA("ProviderNSA")
    header.setRequesterNSA("RequesterNSA")

    val doc = NsiMessage.db.newDocument()
    NsiMessage.marshaller.marshal(factory.createNsiHeader(header), doc)
    doc
  }
}
object NsiMessage {

  def db = DocumentBuilderFactory.newInstance().newDocumentBuilder()

  def marshaller = JAXBContext.newInstance("org.ogf.schemas.nsi._2013._04.framework.types:org.ogf.schemas.nsi._2013._04.connection.types:org.ogf.schemas.nsi._2013._04.connection._interface:org.ogf.schemas.nsi._2013._04.framework.headers").createMarshaller
}
