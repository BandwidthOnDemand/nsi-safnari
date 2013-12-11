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

case class StoredMessage(correlationId: CorrelationId, direction: String, protocol: String, tpe: String, content: String, createdAt: Instant = new Instant())

object StoredMessage {
  private def conversionToFormat[A, B: Format](conversion: Conversion[A, B]): Format[A] = new Format[A] {
    override def reads(js: JsValue): JsResult[A] = Json.fromJson[B](js).flatMap { b =>
      conversion.invert(b).fold(error => JsError(ValidationError("error.conversion.failed", error)), JsSuccess(_))
    }
    override def writes(a: A): JsValue = Json.toJson(conversion(a).fold(error => sys.error(s"failed to write message $a: $error"), identity))
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

  private implicit val CorrelationIdFormat: Format[CorrelationId] = new Format[CorrelationId] {
    override def reads(json: JsValue): JsResult[CorrelationId] = json match {
      case JsString(s) => CorrelationId.fromString(s) match {
        case Some(correlationId) => JsSuccess(correlationId)
        case None                => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.correlationId", s))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
    override def writes(correlationId: CorrelationId): JsValue = JsString(correlationId.toString)
  }
  private implicit val UriFormat: Format[URI] = new Format[URI] {
    override def reads(json: JsValue): JsResult[URI] = json match {
      case JsString(s) => Try(URI.create(s)) match {
        case Success(uri) => JsSuccess(uri)
        case Failure(_)   => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.uri", s))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
    override def writes(uri: URI): JsValue = JsString(uri.toASCIIString())
  }

  // Json.format doesn't work, so use manual conversion instead.
  private implicit val FromRequesterFormat = ((__ \ 'message).format[NsiProviderMessage[NsiProviderOperation]]).inmap(FromRequester.apply, unlift(FromRequester.unapply))
  private implicit val ToRequesterFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(ToRequester.apply, unlift(ToRequester.unapply))
  private implicit val FromProviderFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(FromProvider.apply, unlift(FromProvider.unapply))
  private implicit val AckFromProviderFormat = ((__ \ 'message).format[NsiProviderMessage[NsiAcknowledgement]]).inmap(AckFromProvider.apply, unlift(AckFromProvider.unapply))
  private implicit val ToProviderFormat = Json.format[ToProvider]
  private implicit val FromPceFormat = Json.format[FromPce]
  private implicit val ToPceFormat = Json.format[ToPce]
  private implicit val MessageDeliveryFailureFormat = Json.format[MessageDeliveryFailure]
  private implicit val PassedEndTimeFormat = Json.format[PassedEndTime]

  implicit val MessageToStoredMessage = Conversion.build[Message, StoredMessage] {
    case message @ FromRequester(nsi)    => Right(StoredMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "FromRequester", formatJson(message)))
    case message @ ToRequester(nsi)      => Right(StoredMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "ToRequester", formatJson(message)))
    case message @ FromProvider(nsi)     => Right(StoredMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "FromProvider", formatJson(message)))
    case message @ AckFromProvider(nsi)  => Right(StoredMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "ProviderAck", formatJson(message)))
    case message @ ToProvider(nsi, _)    => Right(StoredMessage(nsi.headers.correlationId, directionOf(message), "NSIv2", "ToProvider", formatJson(message)))
    case message @ FromPce(pce)          => Right(StoredMessage(pce.correlationId, directionOf(message), "PCEv1", "FromPce", formatJson(message)))
    case message @ ToPce(pce)            => Right(StoredMessage(pce.correlationId, directionOf(message), "PCEv1", "ToPce", formatJson(message)))
    case message: MessageDeliveryFailure => Right(StoredMessage(message.correlationId, directionOf(message), "", "MessageDeliveryFailure", formatJson(message)))
    case message: PassedEndTime          => Right(StoredMessage(message.correlationId, directionOf(message), "", "PassedEndTime", formatJson(message)))
  } { stored =>
    stored.tpe match {
      case "FromRequester"          => parseJson[FromRequester](stored.content)
      case "ToRequester"            => parseJson[ToRequester](stored.content)
      case "FromProvider"           => parseJson[FromProvider](stored.content)
      case "ProviderAck"            => parseJson[AckFromProvider](stored.content)
      case "ToProvider"             => parseJson[ToProvider](stored.content)
      case "FromPce"                => parseJson[FromPce](stored.content)
      case "ToPce"                  => parseJson[ToPce](stored.content)
      case "MessageDeliveryFailure" => parseJson[MessageDeliveryFailure](stored.content)
      case "PassedEndTime"          => parseJson[PassedEndTime](stored.content)
    }
  }

  private def formatJson[T: Writes](value: T): String = Json.stringify(Json.toJson(value))
  private def parseJson[T: Reads](json: String): Either[String, T] = Json.parse(json).validate[T].fold(errors => Left(errors.mkString(", ")), ok => Right(ok))
}

class MessageStore[T]()(implicit conversion: Conversion[T, StoredMessage]) {
  private implicit def rowToUuid: Column[UUID] = {
    Column.nonNull[UUID] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case uuid: UUID => Right(uuid)
        case _          => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to UUID for column " + qualified))
      }
    }
  }

  def storeInboundWithOutboundMessages(aggregatedConnectionId: ConnectionId, inbound: T, outbound: Seq[T]) = DB.withTransaction { implicit connection =>
    val inboundId = store(aggregatedConnectionId, inbound, None)
    outbound.foreach(store(aggregatedConnectionId, _, Some(inboundId)))
  }

  def loadAll(aggregatedConnectionId: ConnectionId): Seq[T] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT correlation_id, direction, protocol, type, content, created_at
          FROM messages
         WHERE aggregated_connection_id = {aggregated_connection_id}
         ORDER BY id ASC""").on(
      'aggregated_connection_id -> aggregatedConnectionId).as(messageParser.*).map(message => conversion.invert(message).fold(sys.error, identity)) // FIXME error handling
  }

  def loadEverything(): Seq[(ConnectionId, Seq[T])] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT aggregated_connection_id, direction, correlation_id, protocol, type, content, created_at
          FROM messages
         ORDER BY aggregated_connection_id ASC, id ASC""").as(
      (str("aggregated_connection_id") ~ messageParser).*).groupBy {
        case connectionId ~ _ => connectionId
      }.map {
        case (connectionId, messages) =>
          connectionId -> messages.flatMap {
            case _ ~ message => conversion.invert(message).right.toOption
          }
      }(collection.breakOut)
  }

  private def messageParser = (get[UUID]("correlation_id") ~ str("direction") ~ str("protocol") ~ str("type") ~ str("content") ~ get[java.util.Date]("created_at")).map {
    case correlationId ~ direction ~ protocol ~ tpe ~ content ~ createdAt =>
      StoredMessage(CorrelationId.fromUuid(correlationId), direction, protocol, tpe, content, new Instant(createdAt))
  }

  private def store(aggregatedConnectionId: ConnectionId, message: T, inboundId: Option[Long]) = DB.withTransaction { implicit connection =>
    val stored = conversion(message).right.get
    SQL("""
        INSERT INTO messages (aggregated_connection_id, correlation_id, direction, protocol, type, content, created_at, inbound_message_id)
             VALUES ({aggregated_connection_id}, {correlation_id}, {direction}, {protocol}, {type}, {content}, {created_at}, {inbound_message_id})
        """).on(
      'aggregated_connection_id -> aggregatedConnectionId,
      'direction -> stored.direction,
      'correlation_id -> stored.correlationId.value,
      'protocol -> stored.protocol,
      'type -> stored.tpe,
      'content -> stored.content,
      'created_at -> stored.createdAt.toSqlTimestamp,
      'inbound_message_id -> inboundId).executeInsert().getOrElse(sys.error("insert failed to generate primary key"))
  }
}
