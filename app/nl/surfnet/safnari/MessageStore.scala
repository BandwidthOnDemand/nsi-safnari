package nl.surfnet.safnari

import anorm.SqlParser._
import anorm._
import java.net.URI
import java.util.UUID
import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._
import soap.NsiSoapConversions._
import org.joda.time.Instant
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.w3c.dom.Document
import play.api.Logger
import play.api.data.validation.ValidationError
import play.api.db.DB
import play.api.libs.functional.FunctionalBuilder
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.util.{ Try, Success, Failure }

case class SerializedMessage(correlationId: CorrelationId, direction: String, protocol: String, tpe: String, content: String)
object SerializedMessage {
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

  implicit val MessageToSerializedMessage = Conversion.build[Message, SerializedMessage] {
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

case class MessageRecord[T](id: Long, createdAt: Instant, aggregatedConnectionId: ConnectionId, message: T) {
  def map[B](f: T => B) = copy(message = f(message))
}

class MessageStore(implicit app: play.api.Application) {
  import SerializedMessage.MessageToSerializedMessage

  private implicit def rowToUuid: Column[UUID] = {
    Column.nonNull[UUID] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case uuid: UUID => Right(uuid)
        case _          => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to UUID for column " + qualified))
      }
    }
  }

  def create(aggregatedConnectionId: ConnectionId, createdAt: Instant, requesterNsa: RequesterNsa): Unit = DB.withTransaction { implicit connection =>
    SQL("""INSERT INTO connections (aggregated_connection_id, created_at, requester_nsa) VALUES ({aggregated_connection_id}, {created_at}, {requester_nsa})""")
      .on('aggregated_connection_id -> aggregatedConnectionId, 'created_at -> createdAt.toSqlTimestamp, 'requester_nsa -> requesterNsa)
      .executeInsert()
  }

  def storeInboundWithOutboundMessages(aggregatedConnectionId: ConnectionId, createdAt: Instant, inbound: InboundMessage, outbound: Seq[OutboundMessage]) = DB.withTransaction { implicit connection =>
    val connectionPk = SQL("""SELECT id FROM connections WHERE aggregated_connection_id = {aggregated_connection_id} AND deleted_at IS NULL""")
      .on('aggregated_connection_id -> aggregatedConnectionId)
      .as(get[Long]("id").singleOpt)
      .getOrElse {
        throw new IllegalArgumentException(s"connection $aggregatedConnectionId does not exist or is already deleted")
      }
    val inboundId = store(connectionPk, createdAt, inbound, None)
    outbound.foreach(store(connectionPk, createdAt, _, Some(inboundId)))
  }

  def loadAll(aggregatedConnectionId: ConnectionId): Seq[MessageRecord[Message]] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT m.id, c.aggregated_connection_id, m.created_at, m.correlation_id, m.direction, m.protocol, m.type, m.content
          FROM messages m INNER JOIN connections c ON m.connection_id = c.id
         WHERE c.aggregated_connection_id = {aggregated_connection_id}
         ORDER BY m.id ASC""").on(
      'aggregated_connection_id -> aggregatedConnectionId)
      .as(recordParser.*)
      .map(_.map(message => MessageToSerializedMessage.invert(message).get)) // FIXME error handling
  }

  def loadEverything(): Seq[(ConnectionId, Seq[MessageRecord[Message]])] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT m.id, c.aggregated_connection_id, m.created_at, m.correlation_id, m.direction, m.protocol, m.type, m.content
          FROM messages m INNER JOIN connections c ON m.connection_id = c.id
         WHERE c.deleted_at IS NULL
         ORDER BY c.aggregated_connection_id ASC, m.id ASC""")
      .as(recordParser.*)
      .groupBy(_.aggregatedConnectionId)
      .map {
        case (connectionId, records) =>
          connectionId -> records.flatMap { record =>
            val deserialized = MessageToSerializedMessage.invert(record.message).toOption
            deserialized.map(message => record.map(Function.const(message)))
          }
      }(collection.breakOut)
  }

  def delete(aggregatedConnectionId: ConnectionId, deletedAt: Instant): Unit = DB.withTransaction { implicit connection =>
    SQL("""UPDATE connections SET deleted_at = {deleted_at} WHERE aggregated_connection_id = {aggregated_connection_id} AND deleted_at IS NULL""")
      .on('aggregated_connection_id -> aggregatedConnectionId, 'deleted_at -> deletedAt.toSqlTimestamp)
      .executeUpdate().tap(n => Logger.debug(s"Deleted connection $aggregatedConnectionId"))
  }

  private def recordParser = (get[Long]("id") ~ get[java.util.Date]("created_at") ~ get[String]("aggregated_connection_id") ~ get[UUID]("correlation_id") ~ str("direction") ~ str("protocol") ~ str("type") ~ str("content")).map {
    case id ~ createdAt ~ aggregatedConnectionId ~ correlationId ~ direction ~ protocol ~ tpe ~ content =>
      MessageRecord(id, new Instant(createdAt), aggregatedConnectionId, SerializedMessage(CorrelationId.fromUuid(correlationId), direction, protocol, tpe, content))
  }

  private def store(connectionPk: Long, createdAt: Instant, message: Message, inboundId: Option[Long]) = DB.withTransaction { implicit connection =>
    val serialized = MessageToSerializedMessage(message).get
    SQL("""
        INSERT INTO messages (connection_id, correlation_id, direction, protocol, type, content, created_at, inbound_message_id)
             VALUES ({connection_id}, uuid({correlation_id}), {direction}, {protocol}, {type}, {content}, {created_at}, {inbound_message_id})
        """).on(
      'connection_id -> connectionPk,
      'direction -> serialized.direction,
      'correlation_id -> serialized.correlationId.value,
      'protocol -> serialized.protocol,
      'type -> serialized.tpe,
      'content -> serialized.content,
      'created_at -> createdAt.toSqlTimestamp,
      'inbound_message_id -> inboundId).executeInsert().getOrElse(sys.error("insert failed to generate primary key"))
  }
}
