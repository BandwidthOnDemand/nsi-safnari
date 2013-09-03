package nl.surfnet.safnari

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

final case class FromPce(message: PceResponse) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
final case class ToPce(message: PceRequest) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
