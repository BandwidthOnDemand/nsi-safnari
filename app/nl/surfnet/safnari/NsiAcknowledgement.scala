package nl.surfnet.safnari

import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.connection.types.{ ObjectFactory => TypesObjectFactory }
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryConfirmedType
import org.ogf.schemas.nsi._2013._04.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveResponseType
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
import scala.collection.JavaConverters._
import nl.surfnet.safnari.NsiMessage.marshal
import NsiAcknowledgement.factory

sealed trait NsiAcknowledgement extends NsiMessage {
  override def optionalConnectionId: Option[ConnectionId] = None
}

object NsiAcknowledgement {
  private[safnari] val factory = new TypesObjectFactory()
}

case class GenericAck(correlationId: CorrelationId) extends NsiAcknowledgement {
  override def asDocument = {
    val ack = factory.createGenericAcknowledgmentType()
    marshal(factory.createAcknowledgment(ack))
  }
}

case class ReserveResponse(correlationId: CorrelationId, connectionId: String) extends NsiAcknowledgement {
  override def optionalConnectionId = Some(connectionId)
  override def asDocument = {
    val response = new ReserveResponseType()
    response.setConnectionId(connectionId)

    marshal(factory.createReserveResponse(response))
  }
}

case class ServiceException(correlationId: CorrelationId, text: String) extends NsiAcknowledgement {
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

case class QuerySummarySyncConfirmed(correlationId: CorrelationId, results: Seq[QuerySummaryResultType]) extends NsiAcknowledgement {
  override def asDocument: Document = {
    val response = new QuerySummaryConfirmedType().withReservation(results.asJava)

    marshal(factory.createQuerySummarySyncConfirmed(response))
  }
}
