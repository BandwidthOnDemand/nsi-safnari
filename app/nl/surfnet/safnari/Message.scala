package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

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

final case class ToRequester(message: NsiRequesterMessage[NsiRequesterOperation]) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class FromRequester(message: NsiProviderMessage[NsiProviderOperation]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}

final case class ToProvider(message: NsiProviderMessage[NsiProviderOperation], provider: ProviderEndPoint) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class FromProvider(message: NsiRequesterMessage[NsiRequesterOperation]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class AckFromProvider(message: NsiProviderMessage[NsiAcknowledgement]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class ErrorFromProvider(requestHeaders: NsiHeaders, ackHeaders: Option[NsiHeaders], errorMessage: String, serviceException: Option[ServiceExceptionType]) extends InboundMessage {
  override def toShortString = s"ErrorFromProvider(cid=$correlationId, $errorMessage, ${serviceException.map(_.getClass().getSimpleName())})"
  override def correlationId = ackHeaders.map(_.correlationId).getOrElse(requestHeaders.correlationId)
}

final case class FromPce(message: PceResponse) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
final case class ToPce(message: PceRequest) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
