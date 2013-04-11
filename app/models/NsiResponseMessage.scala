package models

import org.w3c.dom.Document
import javax.xml.bind.JAXBContext
import javax.xml.parsers.DocumentBuilderFactory
import org.ogf.schemas.nsi._2013._04.connection._interface.GenericAcknowledgmentType
import org.ogf.schemas.nsi._2013._04.connection._interface.{ ObjectFactory => InterfaceObjectFactory }
import org.ogf.schemas.nsi._2013._04.connection.types.{ ObjectFactory => TypesObjectFactory }
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._04.framework.headers.{ ObjectFactory => HeadersObjectFactory }

sealed trait NsiResponseMessage extends NsiMessage

object NsiResponseMessage {
  import NsiMessage._

  case class GenericAck(headers: NsiHeaders) extends NsiResponseMessage {
    override def bodyDocument = {
      val factory = new InterfaceObjectFactory()
      val ack = factory.createGenericAcknowledgmentType()
      marshal(factory.createAcknowledgment(ack))
    }
  }

  case class ReserveResponse(headers: NsiHeaders, connectionId: String) extends NsiResponseMessage {
    override def bodyDocument = {
      val factory = new TypesObjectFactory()
      val response = factory.createReserveResponseType()
      response.setConnectionId(connectionId)

      marshal(factory.createReserveResponse(response))
    }
  }

  case class ServiceException(headers: NsiHeaders) extends NsiResponseMessage {
    override def bodyDocument: Document = ???
  }
}
