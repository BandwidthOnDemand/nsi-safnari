package models

import org.w3c.dom.Document
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import org.ogf.schemas.nsi._2013._04.connection._interface.GenericAcknowledgmentType
import org.ogf.schemas.nsi._2013._04.connection._interface.{ObjectFactory => InterfaceObjectFactory}
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._04.framework.headers.{ObjectFactory => HeadersObjectFactory}

sealed trait NsiResponseMessage {
  def bodyDocument: Document
  def headerDocument: Document
}

object NsiResponseMessage {

  lazy val marshaller = JAXBContext.newInstance(
    classOf[GenericAcknowledgmentType],
    classOf[CommonHeaderType]).createMarshaller

  case class GenericAck(correlationId: String) extends NsiResponseMessage {
    lazy val db = DocumentBuilderFactory.newInstance().newDocumentBuilder()

    def bodyDocument = {
      val factory = new InterfaceObjectFactory()
      val ack = factory.createGenericAcknowledgmentType()

      val doc = db.newDocument()
      marshaller.marshal(factory.createAcknowledgment(ack), doc)
      doc
    }

    def headerDocument = {
      val factory = new HeadersObjectFactory()
      val header = factory.createCommonHeaderType()
      header.setCorrelationId(correlationId)

      val doc = db.newDocument()
      marshaller.marshal(factory.createNsiHeader(header), doc)
      doc
    }
  }

  case class GenericFail() extends NsiResponseMessage {
    def bodyDocument = ???

    def headerDocument = ???
  }
}