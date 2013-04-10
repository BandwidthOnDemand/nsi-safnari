package models

import org.w3c.dom.Document
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import org.ogf.schemas.nsi._2013._04.connection._interface.GenericAcknowledgmentType
import org.ogf.schemas.nsi._2013._04.connection._interface.{ObjectFactory => InterfaceObjectFactory}
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._04.framework.headers.{ObjectFactory => HeadersObjectFactory}

sealed trait NsiResponseMessage {
  def bodyDocument: Document = ???
  def headerDocument: Document = ???
}

object NsiResponseMessage {

  lazy val marshaller = JAXBContext.newInstance(
    classOf[GenericAcknowledgmentType],
    classOf[CommonHeaderType]).createMarshaller

  case class GenericAck(correlationId: String) extends NsiResponseMessage {
    lazy val db = DocumentBuilderFactory.newInstance().newDocumentBuilder()

    override def bodyDocument = {
      val factory = new InterfaceObjectFactory()
      val ack = factory.createGenericAcknowledgmentType()

      val doc = db.newDocument()
      marshaller.marshal(factory.createAcknowledgment(ack), doc)
      doc
    }

    override def headerDocument = {
      val factory = new HeadersObjectFactory()
      val header = factory.createCommonHeaderType()
      header.setCorrelationId(correlationId)

      val doc = db.newDocument()
      marshaller.marshal(factory.createNsiHeader(header), doc)
      doc
    }
  }

  case class GenericFail() extends NsiResponseMessage

  case class ReserveConfirmed() extends NsiResponseMessage
  case class ReserveFailed() extends NsiResponseMessage
  case class ReserveCommitConfirmed() extends NsiResponseMessage
  case class ReserveCommitFailed() extends NsiResponseMessage
  case class ReserveAbortConfirmed() extends NsiResponseMessage
  case class ReserveTimeout() extends NsiResponseMessage

  case class ProvisionConfirmed() extends NsiResponseMessage
  case class ReleaseConfirmed() extends NsiResponseMessage
  case class TermianteConfirmed() extends NsiResponseMessage

  case class QuerySummaryConfirmed() extends NsiResponseMessage
  case class QuerySummaryFailed() extends NsiResponseMessage
  case class QueryRecursiveConfirmed() extends NsiResponseMessage
  case class QueryRecursiveFailed() extends NsiResponseMessage

  case class ErrorEvent() extends NsiResponseMessage
  case class DataPlaneStateChanged() extends NsiResponseMessage
  case class MesasgeDeliveryTimeout() extends NsiResponseMessage
}