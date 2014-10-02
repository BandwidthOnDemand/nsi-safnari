package nl.surfnet.safnari

import java.net.URI
import nl.surfnet.nsiv2.messages._
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

sealed trait Message {
  def toShortString: String
  def correlationId: CorrelationId
}
object Message {
  private[safnari] def shortString(messageType: Class[_], operationType: Class[_], correlationId: CorrelationId) =
    s"${messageType.getSimpleName()}(cid=$correlationId, ${operationType.getSimpleName()})"
}

sealed trait OutboundMessage extends Message
sealed trait InboundMessage extends Message

final case class FromRequester(message: NsiProviderMessage[NsiProviderOperation]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class ToRequester(message: NsiRequesterMessage[NsiRequesterOperation]) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}

final case class ToProvider(message: NsiProviderMessage[NsiProviderOperation], provider: ProviderEndPoint) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class AckFromProvider(message: NsiProviderMessage[NsiAcknowledgement]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class FromProvider(message: NsiRequesterMessage[NsiRequesterOperation]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}

final case class FromPce(message: PceResponse) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
final case class AckFromPce(message: PceAcknowledgement) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
final case class ToPce(message: PceRequest) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}

final case class MessageDeliveryFailure(override val correlationId: CorrelationId, connectionId: Option[ConnectionId], originalCorrelationId: CorrelationId, uri: URI, timestamp: DateTime, message: String) extends InboundMessage {
  override def toShortString = s"${getClass().getSimpleName()}(correlationId=$correlationId, connectionId=$connectionId, originalCorrelationId=$originalCorrelationId, uri=$uri, timestamp=$timestamp, message=$message)"
}
final case class PassedEndTime(override val correlationId: CorrelationId, connectionId: ConnectionId, timestamp: DateTime) extends InboundMessage {
  override def toShortString = s"${getClass().getSimpleName()}(correlationId=$correlationId, connectionId=$connectionId, timestamp=$timestamp)"
}
