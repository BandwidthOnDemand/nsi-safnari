/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package nl.surfnet.safnari

import nl.surfnet.nsiv2.messages.{given, *}
import nl.surfnet.nsiv2.persistence.{MessageData, given}
import nl.surfnet.nsiv2.soap.Conversion
import play.api.libs.json.*
import scala.util.Success

object MessagePersistence {
  import MessageData.*
  import PceMessage.ProviderEndPointFormat

  // Json.format doesn't work, so use manual conversion instead.
  implicit val FromRequesterFormat: OFormat[FromRequester] =
    unaryCaseClassFormat("message")(FromRequester.apply, _.message)
  implicit val ToRequesterFormat: OFormat[ToRequester] =
    unaryCaseClassFormat("message")(ToRequester.apply, _.message)
  implicit val FromProviderFormat: OFormat[FromProvider] =
    unaryCaseClassFormat("message")(FromProvider.apply, _.message)
  implicit val AckFromProviderFormat: OFormat[AckFromProvider] =
    unaryCaseClassFormat("message")(AckFromProvider.apply, _.message)
  implicit val ToProviderFormat: OFormat[ToProvider] = Json.format[ToProvider]
  implicit val FromPceFormat: OFormat[FromPce] = Json.format[FromPce]
  implicit val AckFromPceFormat: OFormat[AckFromPce] = Json.format[AckFromPce]
  implicit val ToPceFormat: OFormat[ToPce] = Json.format[ToPce]
  implicit val MessageDeliveryFailureFormat: OFormat[MessageDeliveryFailure] =
    Json.format[MessageDeliveryFailure]
  implicit val PassedEndTimeFormat: OFormat[PassedEndTime] = Json.format[PassedEndTime]

  implicit val MessageToMessageData: Conversion[Message, MessageData] =
    Conversion.build[Message, MessageData] {
      case message @ FromRequester(nsi) =>
        Success(MessageData(Some(nsi.headers.correlationId), "FromRequester", formatJson(message)))
      case message @ ToRequester(nsi) =>
        Success(MessageData(Some(nsi.headers.correlationId), "ToRequester", formatJson(message)))
      case message @ FromProvider(nsi) =>
        Success(MessageData(Some(nsi.headers.correlationId), "FromProvider", formatJson(message)))
      case message @ AckFromProvider(nsi) =>
        Success(MessageData(Some(nsi.headers.correlationId), "ProviderAck", formatJson(message)))
      case message @ ToProvider(nsi, _) =>
        Success(MessageData(Some(nsi.headers.correlationId), "ToProvider", formatJson(message)))
      case message @ FromPce(pce) =>
        Success(MessageData(Some(pce.correlationId), "FromPce", formatJson(message)))
      case message @ AckFromPce(pce) =>
        Success(MessageData(Some(pce.correlationId), "AckFromPce", formatJson(message)))
      case message @ ToPce(pce) =>
        Success(MessageData(Some(pce.correlationId), "ToPce", formatJson(message)))
      case message: MessageDeliveryFailure =>
        Success(
          MessageData(Some(message.correlationId), "MessageDeliveryFailure", formatJson(message))
        )
      case message: PassedEndTime =>
        Success(MessageData(Some(message.correlationId), "PassedEndTime", formatJson(message)))
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
