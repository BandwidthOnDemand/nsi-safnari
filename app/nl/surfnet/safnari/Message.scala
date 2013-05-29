package nl.surfnet.safnari

import java.net.URI
import play.api.libs.json._
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import scala.util.Try

sealed trait Message
sealed trait InboundMessage extends Message
sealed trait OutboundMessage extends Message
case class FromRequester(message: NsiProviderOperation) extends InboundMessage
case class ToRequester(message: NsiRequesterOperation) extends OutboundMessage
case class FromProvider(message: NsiRequesterOperation) extends InboundMessage
case class ToProvider(message: NsiProviderOperation, provider: ProviderEndPoint) extends OutboundMessage
case class FromPce(message: PceResponse) extends InboundMessage
case class ToPce(message: PceRequest) extends OutboundMessage
