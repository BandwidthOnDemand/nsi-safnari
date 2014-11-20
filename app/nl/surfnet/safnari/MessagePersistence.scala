package nl.surfnet.safnari

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.persistence.MessageData
import nl.surfnet.nsiv2.soap.Conversion
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.util.{ Try, Success, Failure }

object MessagePersistence {
  import MessageData._
  import PceMessage.ProviderEndPointFormat

  // Json.format doesn't work, so use manual conversion instead.
  implicit val FromRequesterFormat = ((__ \ 'message).format[NsiProviderMessage[NsiProviderOperation]]).inmap(FromRequester.apply, unlift(FromRequester.unapply))
  implicit val ToRequesterFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(ToRequester.apply, unlift(ToRequester.unapply))
  implicit val FromProviderFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(FromProvider.apply, unlift(FromProvider.unapply))
  implicit val AckFromProviderFormat = ((__ \ 'message).format[NsiProviderMessage[NsiAcknowledgement]]).inmap(AckFromProvider.apply, unlift(AckFromProvider.unapply))
  implicit val ToProviderFormat = Json.format[ToProvider]
  implicit val FromPceFormat = Json.format[FromPce]
  implicit val AckFromPceFormat = Json.format[AckFromPce]
  implicit val ToPceFormat = Json.format[ToPce]
  implicit val MessageDeliveryFailureFormat = Json.format[MessageDeliveryFailure]
  implicit val PassedEndTimeFormat = Json.format[PassedEndTime]

  implicit val MessageToMessageData = Conversion.build[Message, MessageData] {
    case message @ FromRequester(nsi)    => Success(MessageData(nsi.headers.correlationId, "FromRequester", formatJson(message)))
    case message @ ToRequester(nsi)      => Success(MessageData(nsi.headers.correlationId, "ToRequester", formatJson(message)))
    case message @ FromProvider(nsi)     => Success(MessageData(nsi.headers.correlationId, "FromProvider", formatJson(message)))
    case message @ AckFromProvider(nsi)  => Success(MessageData(nsi.headers.correlationId, "ProviderAck", formatJson(message)))
    case message @ ToProvider(nsi, _)    => Success(MessageData(nsi.headers.correlationId, "ToProvider", formatJson(message)))
    case message @ FromPce(pce)          => Success(MessageData(pce.correlationId, "FromPce", formatJson(message)))
    case message @ AckFromPce(pce)       => Success(MessageData(pce.correlationId, "AckFromPce", formatJson(message)))
    case message @ ToPce(pce)            => Success(MessageData(pce.correlationId, "ToPce", formatJson(message)))
    case message: MessageDeliveryFailure => Success(MessageData(message.correlationId, "MessageDeliveryFailure", formatJson(message)))
    case message: PassedEndTime          => Success(MessageData(message.correlationId, "PassedEndTime", formatJson(message)))
  } { serialized =>
    serialized.tpe match {
      case "FromRequester"          => parseJson[FromRequester](serialized.content)
      case "ToRequester"            => parseJson[ToRequester](serialized.content)
      case "FromProvider"           => parseJson[FromProvider](serialized.content)
      case "ProviderAck"            => parseJson[AckFromProvider](serialized.content)
      case "ToProvider"             => parseJson[ToProvider](serialized.content)
      case "AckFromPce"             => parseJson[AckFromPce](serialized.content)
      case "FromPce"                => parseJson[FromPce](serialized.content)
      case "ToPce"                  => parseJson[ToPce](serialized.content)
      case "MessageDeliveryFailure" => parseJson[MessageDeliveryFailure](serialized.content)
      case "PassedEndTime"          => parseJson[PassedEndTime](serialized.content)
    }
  }
}
