package nl.surfnet.safnari

import anorm._
import anorm.SqlParser._
import java.util.UUID
import nl.surfnet.safnari.NsiSoapConversions._
import org.joda.time.Instant
import org.w3c.dom.Document
import play.api.Play.current
import play.api.data.validation.ValidationError
import play.api.db.DB
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.functional.FunctionalBuilder
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import scala.util.{ Try, Success, Failure }
import java.net.URI

private case class SerializedMessage(correlationId: CorrelationId, direction: String, protocol: String, tpe: String, content: String)

case class MessageRecord[T](id: Long, createdAt: Instant = new Instant(), aggregatedConnectionId: ConnectionId, message: T) {
  def map[B](f: T => B) = copy(message = f(message))
}

object MessageStore {
  private def conversionToFormat[A, B: Format](conversion: Conversion[A, B]): Format[A] = new Format[A] {
    override def reads(js: JsValue): JsResult[A] = Json.fromJson[B](js).flatMap { b =>
      conversion.invert(b).toEither.fold(error => JsError(ValidationError("error.conversion.failed", error)), JsSuccess(_))
    }
    override def writes(a: A): JsValue = Json.toJson(conversion(a).get)
  }

  private val Inbound = "INBOUND"
  private val Outbound = "OUTBOUND"
  private def directionOf(message: Message): String = message match {
    case _: InboundMessage  => Inbound
    case _: OutboundMessage => Outbound
  }

  implicit val NsiProviderOperationFormat: Format[NsiProviderMessage[NsiProviderOperation]] = conversionToFormat(NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(DocumentToString))
  implicit val NsiProviderAckFormat: Format[NsiProviderMessage[NsiAcknowledgement]] = conversionToFormat(NsiProviderMessageToDocument[NsiAcknowledgement](None).andThen(DocumentToString))
  implicit val NsiRequesterOperationFormat: Format[NsiRequesterMessage[NsiRequesterOperation]] = conversionToFormat(NsiRequesterMessageToDocument[NsiRequesterOperation](None).andThen(DocumentToString))

  import PceMessage.ProviderEndPointFormat

  private implicit val NsiHeadersFormat: Format[NsiHeaders] = conversionToFormat(Conversion[NsiHeaders, String])
  private implicit val ServiceExceptionTypeFormat: Format[ServiceExceptionType] = conversionToFormat(Conversion[ServiceExceptionType, String])

  // Json.format doesn't work, so use manual conversion instead.
  private implicit val FromRequesterFormat = ((__ \ 'message).format[NsiProviderMessage[NsiProviderOperation]]).inmap(FromRequester.apply, unlift(FromRequester.unapply))
  private implicit val ToRequesterFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(ToRequester.apply, unlift(ToRequester.unapply))
  private implicit val FromProviderFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(FromProvider.apply, unlift(FromProvider.unapply))
  private implicit val AckFromProviderFormat = ((__ \ 'message).format[NsiProviderMessage[NsiAcknowledgement]]).inmap(AckFromProvider.apply, unlift(AckFromProvider.unapply))
  private implicit val ToProviderFormat = Json.format[ToProvider]
  private implicit val FromPceFormat = Json.format[FromPce]
  private implicit val AckFromPceFormat = Json.format[AckFromPce]
  private implicit val ToPceFormat = Json.format[ToPce]
  private implicit val MessageDeliveryFailureFormat = Json.format[MessageDeliveryFailure]
  private implicit val PassedEndTimeFormat = Json.format[PassedEndTime]

  private implicit val MessageToSerializedMessage = Conversion.build[Message, SerializedMessage] {
    case message @ FromRequester(nsi)    => Success(SerializedMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "FromRequester", formatJson(message)))
    case message @ ToRequester(nsi)      => Success(SerializedMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "ToRequester", formatJson(message)))
    case message @ FromProvider(nsi)     => Success(SerializedMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "FromProvider", formatJson(message)))
    case message @ AckFromProvider(nsi)  => Success(SerializedMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "ProviderAck", formatJson(message)))
    case message @ ToProvider(nsi, _)    => Success(SerializedMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "ToProvider", formatJson(message)))
    case message @ FromPce(pce)          => Success(SerializedMessage(pce.correlationId, directionOf(message), "PCEv1", "FromPce", formatJson(message)))
    case message @ AckFromPce(pce)       => Success(SerializedMessage(pce.correlationId, directionOf(message), "PCEv1", "AckFromPce", formatJson(message)))
    case message @ ToPce(pce)            => Success(SerializedMessage(pce.correlationId, directionOf(message), "PCEv1", "ToPce", formatJson(message)))
    case message: MessageDeliveryFailure => Success(SerializedMessage(message.correlationId, directionOf(message), "", "MessageDeliveryFailure", formatJson(message)))
    case message: PassedEndTime          => Success(SerializedMessage(message.correlationId, directionOf(message), "", "PassedEndTime", formatJson(message)))
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

  private def formatJson[T: Writes](value: T): String = Json.stringify(Json.toJson(value))
  private def parseJson[T: Reads](json: String): Try[T] = Json.parse(json).validate[T].fold(errors => Failure(ErrorMessageException(errors.mkString(", "))), ok => Success(ok))
}

class MessageStore() {
  import MessageStore.MessageToSerializedMessage

  private implicit def rowToUuid: Column[UUID] = {
    Column.nonNull[UUID] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case uuid: UUID => Right(uuid)
        case _          => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to UUID for column " + qualified))
      }
    }
  }

  def storeInboundWithOutboundMessages(aggregatedConnectionId: ConnectionId, inbound: InboundMessage, outbound: Seq[OutboundMessage]) = DB.withTransaction { implicit connection =>
    val createdAt = new Instant()
    val inboundId = store(aggregatedConnectionId, createdAt, inbound, None)
    outbound.foreach(store(aggregatedConnectionId, createdAt, _, Some(inboundId)))
  }

  def loadAll(aggregatedConnectionId: ConnectionId): Seq[MessageRecord[Message]] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT id, created_at, aggregated_connection_id, correlation_id, direction, protocol, type, content, created_at
          FROM messages
         WHERE aggregated_connection_id = {aggregated_connection_id}
         ORDER BY id ASC""").on(
      'aggregated_connection_id -> aggregatedConnectionId)
      .as(recordParser.*)
      .map(_.map(message => MessageToSerializedMessage.invert(message).get)) // FIXME error handling
  }

  def loadEverything(): Seq[(ConnectionId, Seq[Message])] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT id, aggregated_connection_id, created_at, direction, correlation_id, protocol, type, content
          FROM messages
         ORDER BY aggregated_connection_id ASC, id ASC""")
      .as(recordParser.*)
      .groupBy(_.aggregatedConnectionId)
      .map {
        case (connectionId, records) =>
          connectionId -> records.flatMap { record =>
            MessageToSerializedMessage.invert(record.message).toOption
          }
      }(collection.breakOut)
  }

  private def recordParser = (get[Long]("id") ~ get[java.util.Date]("created_at") ~ get[String]("aggregated_connection_id") ~ get[UUID]("correlation_id") ~ str("direction") ~ str("protocol") ~ str("type") ~ str("content")).map {
    case id ~ createdAt ~ aggregatedConnectionId ~ correlationId ~ direction ~ protocol ~ tpe ~ content =>
      MessageRecord(id, new Instant(createdAt), aggregatedConnectionId, SerializedMessage(CorrelationId.fromUuid(correlationId), direction, protocol, tpe, content))
  }

  private def store(aggregatedConnectionId: ConnectionId, createdAt: Instant, message: Message, inboundId: Option[Long]) = DB.withTransaction { implicit connection =>
    val serialized = MessageToSerializedMessage(message).get
    SQL("""
        INSERT INTO messages (aggregated_connection_id, correlation_id, direction, protocol, type, content, created_at, inbound_message_id)
             VALUES ({aggregated_connection_id}, {correlation_id}, {direction}, {protocol}, {type}, {content}, {created_at}, {inbound_message_id})
        """).on(
      'aggregated_connection_id -> aggregatedConnectionId,
      'direction -> serialized.direction,
      'correlation_id -> serialized.correlationId.value,
      'protocol -> serialized.protocol,
      'type -> serialized.tpe,
      'content -> serialized.content,
      'created_at -> createdAt.toSqlTimestamp,
      'inbound_message_id -> inboundId).executeInsert().getOrElse(sys.error("insert failed to generate primary key"))
  }
}
