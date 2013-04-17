package nl.surfnet.nsi

import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.connection._interface.{ ObjectFactory => InterfaceObjectFactory }
import org.ogf.schemas.nsi._2013._04.connection.types.{ ObjectFactory => TypesObjectFactory }
import org.ogf.schemas.nsi._2013._04.connection._interface.{ObjectFactory => InterfaceObjectFactory}
import org.ogf.schemas.nsi._2013._04.connection.types.{ObjectFactory => TypesObjectFactory}

sealed trait NsiResponseMessage extends NsiMessage {
  override def optionalConnectionId: Option[ConnectionId] = None
}

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
    override def optionalConnectionId = Some(connectionId)
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
