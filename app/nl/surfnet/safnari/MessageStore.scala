package nl.surfnet.safnari

import anorm._
import anorm.SqlParser._
import java.util.UUID
import nl.surfnet.safnari.NsiSoapConversions._
import org.joda.time.Instant
import org.w3c.dom.Document
import play.api.Play.current
import play.api.db.DB
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.functional.FunctionalBuilder

case class StoredMessage(correlationId: CorrelationId, protocol: String, tpe: String, content: String, createdAt: Instant = new Instant())

object StoredMessage {
  private def conversionToFormat[A, B: Format](conversion: Conversion[A, B]): Format[A] = new Format[A] {
    override def reads(js: JsValue): JsResult[A] = Json.fromJson[B](js).flatMap { b =>
      conversion.invert(b).fold(JsError(_), JsSuccess(_))
    }
    override def writes(a: A): JsValue = Json.toJson(conversion(a).right.get)
  }

  implicit val NsiProviderOperationFormat: Format[NsiProviderMessage[NsiProviderOperation]] = conversionToFormat(Conversion[NsiProviderMessage[NsiProviderOperation], Document] andThen NsiXmlDocumentConversion andThen Conversion[Array[Byte], String])
  implicit val NsiRequesterOperationFormat: Format[NsiRequesterMessage[NsiRequesterOperation]] = conversionToFormat(Conversion[NsiRequesterMessage[NsiRequesterOperation], Document] andThen NsiXmlDocumentConversion andThen Conversion[Array[Byte], String])

  import PceMessage.ProviderEndPointFormat

  // Json.format doesn't work, so use manual conversion instead.
  implicit val FromRequesterFormat = ((__ \ 'message).format[NsiProviderMessage[NsiProviderOperation]]).inmap(FromRequester.apply, unlift(FromRequester.unapply))
  implicit val ToRequesterFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(ToRequester.apply, unlift(ToRequester.unapply))
  implicit val FromProviderFormat = ((__ \ 'message).format[NsiRequesterMessage[NsiRequesterOperation]]).inmap(FromProvider.apply, unlift(FromProvider.unapply))
  implicit val ToProviderFormat = Json.format[ToProvider]
  implicit val FromPceFormat = Json.format[FromPce]
  implicit val ToPceFormat = Json.format[ToPce]

  implicit val MessageToStoredMessage = Conversion.build[Message, StoredMessage] {
    case message @ FromRequester(nsi) => Right(StoredMessage(nsi.headers.correlationId, "NSIv2", "FromRequester", formatJson(message)))
    case message @ ToRequester(nsi)   => Right(StoredMessage(nsi.headers.correlationId, "NSIv2", "ToRequester", formatJson(message)))
    case message @ FromProvider(nsi)  => Right(StoredMessage(nsi.headers.correlationId, "NSIv2", "FromProvider", formatJson(message)))
    case message @ ToProvider(nsi, _) => Right(StoredMessage(nsi.headers.correlationId, "NSIv2", "ToProvider", formatJson(message)))
    case message @ FromPce(pce)       => Right(StoredMessage(pce.correlationId, "PCEv1", "FromPce", formatJson(message)))
    case message @ ToPce(pce)         => Right(StoredMessage(pce.correlationId, "PCEv1", "ToPce", formatJson(message)))
  } { stored =>
    stored.tpe match {
      case "FromRequester" => parseJson[FromRequester](stored.content)
      case "ToRequester"   => parseJson[ToRequester](stored.content)
      case "FromProvider"  => parseJson[FromProvider](stored.content)
      case "ToProvider"    => parseJson[ToProvider](stored.content)
      case "FromPce"       => parseJson[FromPce](stored.content)
      case "ToPce"         => parseJson[ToPce](stored.content)
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

  def store(aggregatedConnectionId: ConnectionId, message: T) = DB.withTransaction { implicit connection =>
    val stored = conversion(message).right.get
    SQL("""
        INSERT INTO messages (aggregated_connection_id, correlation_id, protocol, type, content, created_at)
             VALUES ({aggregated_connection_id}, {correlation_id}, {protocol}, {type}, {content}, {created_at})
        """).on(
      'aggregated_connection_id -> aggregatedConnectionId,
      'correlation_id -> stored.correlationId.value,
      'protocol -> stored.protocol,
      'type -> stored.tpe,
      'content -> stored.content,
      'created_at -> new java.sql.Timestamp(stored.createdAt.getMillis())).executeInsert()
  }

  def storeAll(aggregatedConnectionId: ConnectionId, messages: Seq[T]) = DB.withTransaction { implicit connection =>
    messages.foreach(store(aggregatedConnectionId, _))
  }

  def loadAll(aggregatedConnectionId: ConnectionId): Seq[T] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT correlation_id, protocol, type, content, created_at
          FROM messages
         WHERE aggregated_connection_id = {aggregated_connection_id}
         ORDER BY id ASC""").on(
      'aggregated_connection_id -> aggregatedConnectionId).as(messageParser.*).map(message => conversion.invert(message).fold(sys.error, identity)) // FIXME error handling
  }

  def loadEverything(): Seq[(ConnectionId, Seq[T])] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT aggregated_connection_id, correlation_id, protocol, type, content, created_at
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

  private def messageParser = (get[UUID]("correlation_id") ~ str("protocol") ~ str("type") ~ str("content") ~ get[java.util.Date]("created_at")).map {
    case correlationId ~ protocol ~ tpe ~ content ~ createdAt => StoredMessage(CorrelationId.fromUuid(correlationId), protocol, tpe, content, new Instant(createdAt.getTime()))
  }
}
