package nl.surfnet.safnari

sealed trait Message
sealed trait InboundMessage extends Message
sealed trait OutboundMessage extends Message
final case class FromRequester(message: NsiProviderMessage[NsiProviderOperation]) extends InboundMessage
final case class ToRequester(message: NsiRequesterMessage[NsiRequesterOperation]) extends OutboundMessage
final case class FromProvider(message: NsiRequesterMessage[NsiRequesterOperation]) extends InboundMessage
final case class ToProvider(message: NsiProviderMessage[NsiProviderOperation], provider: ProviderEndPoint) extends OutboundMessage
final case class FromPce(message: PceResponse) extends InboundMessage
final case class ToPce(message: PceRequest) extends OutboundMessage
