package nl.surfnet.nsi

import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.connection.types.{ ObjectFactory => TypesObjectFactory }
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveResponseType
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

sealed trait NsiResponseMessage extends NsiMessage {
  override def optionalConnectionId: Option[ConnectionId] = None
}

object NsiResponseMessage {
  import NsiMessage._

  private val factory = new TypesObjectFactory()

  case class GenericAck(correlationId: CorrelationId) extends NsiResponseMessage {
    override def asDocument = {
      val ack = factory.createGenericAcknowledgmentType()
      marshal(factory.createAcknowledgment(ack))
    }
  }

  case class ReserveResponse(correlationId: CorrelationId, connectionId: String) extends NsiResponseMessage {
    override def optionalConnectionId = Some(connectionId)
    override def asDocument = {
      val response = new ReserveResponseType()
      response.setConnectionId(connectionId)

      marshal(factory.createReserveResponse(response))
    }
  }

  case class ServiceException(correlationId: CorrelationId, text: String) extends NsiResponseMessage {
    override def asDocument: Document = {
      val response = new ServiceExceptionType()
      response.setNsaId("MYNSAID") // TODO
      response.setErrorId("UNKNOWN") // TODO
      response.setText(text)
      response.setVariables(null) // TODO
      // children TODO
      marshal(factory.createServiceException(response))
    }
  }
}
