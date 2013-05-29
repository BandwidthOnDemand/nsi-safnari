package nl.surfnet.safnari

import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.joda.time.DateTimeUtils
import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import play.api.http.Writeable
import java.nio.charset.Charset
import scala.io.Codec
import com.twitter.bijection.Injection
import org.joda.time.Instant
import javax.xml.soap._
import java.io.ByteArrayOutputStream
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import java.net.URI
import java.io.ByteArrayInputStream
import java.util.UUID
import scala.util.Try

case class StoredMessage(correlationId: CorrelationId, protocol: String, tpe: String, content: String, createdAt: Instant = new Instant())

object StoredMessage {

  private val NsiFrameworkHeaderNamespace = "http://schemas.ogf.org/nsi/2013/04/framework/headers"
  private val NsiConnectionTypesNamespace = "http://schemas.ogf.org/nsi/2013/04/connection/types"

  trait NsiRequestMessageFactory[T] {
    type JaxbMessage
    def klass: Class[JaxbMessage]
    def apply(headers: NsiHeaders, body: JaxbMessage): T
  }

  def NsiRequestMessageFactory[M, T](f: (NsiHeaders, M) => T)(implicit manifest: ClassTag[M]) = new NsiRequestMessageFactory[T] {
    override type JaxbMessage = M
    override def klass = manifest.runtimeClass.asInstanceOf[Class[M]]
    override def apply(headers: NsiHeaders, body: M): T = f(headers, body)
  }

  def soapToNsiMessage[T <: NsiMessage](elementToFactory: org.w3c.dom.Element => Either[String, NsiRequestMessageFactory[T]])(soapMessage: SOAPMessage) = {
    val unmarshaller = ToXmlDocument.unmarshaller

    for {
      headerNode <- onlyChildElementWithNamespace(NsiFrameworkHeaderNamespace, soapMessage.getSOAPHeader()).right
      header <- tryEither(unmarshaller.unmarshal(headerNode, classOf[CommonHeaderType]).getValue).right
      protocolVersion <- tryEither(URI.create(header.getProtocolVersion())).right
      correlationId <- CorrelationId.fromString(header.getCorrelationId()).toRight("bad correlation id").right
      replyTo <- tryEither(Option(header.getReplyTo()).map(URI.create)).right
      bodyNode <- onlyChildElementWithNamespace(NsiConnectionTypesNamespace, soapMessage.getSOAPBody()).right
      messageFactory <- elementToFactory(bodyNode).right
      body <- tryEither(unmarshaller.unmarshal(bodyNode, messageFactory.klass).getValue).right
    } yield {
      val headers = NsiHeaders(
        correlationId = correlationId,
        requesterNSA = header.getRequesterNSA(),
        providerNSA = header.getProviderNSA(),
        replyTo = replyTo,
        protocolVersion = protocolVersion)
      messageFactory(headers, body)
    }
  }

  private val MessageProviderFactories = messageFactories(Map(
    "reserve" -> NsiRequestMessageFactory[ReserveType, NsiProviderOperation](Reserve),
    "reserveCommit" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => ReserveCommit(correlationId, body.getConnectionId())),
    "reserveAbort" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => ReserveAbort(correlationId, body.getConnectionId())),
    "provision" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => Provision(correlationId, body.getConnectionId())),
    "release" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => Release(correlationId, body.getConnectionId())),
    "terminate" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => Terminate(correlationId, body.getConnectionId())),
    "querySummary" -> NsiRequestMessageFactory[QueryType, NsiProviderOperation]((correlationId, body) => QuerySummary(correlationId, body.getConnectionId().asScala)),
    "querySummarySync" -> NsiRequestMessageFactory[QueryType, NsiProviderOperation]((correlationId, body) => QuerySummarySync(correlationId, body.getConnectionId().asScala)))) _

  private val MessageRequesterFactories = messageFactories(Map(
    "reserveConfirmed" -> NsiRequestMessageFactory[ReserveConfirmedType, NsiRequesterOperation]((correlationId, body) => ReserveConfirmed(correlationId, body.getConnectionId(), body.getCriteria().asScala.head)),
    "reserveCommitConfirmed" -> NsiRequestMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ReserveCommitConfirmed(correlationId, body.getConnectionId)),
    "provisionConfirmed" -> NsiRequestMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ProvisionConfirmed(correlationId, body.getConnectionId)),
    "releaseConfirmed" -> NsiRequestMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ReleaseConfirmed(correlationId, body.getConnectionId)),
    "terminateConfirmed" -> NsiRequestMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => TerminateConfirmed(correlationId, body.getConnectionId)),
    "dataPlaneStateChange" -> NsiRequestMessageFactory[DataPlaneStateChangeRequestType, NsiRequesterOperation]((correlationId, body) => DataPlaneStateChange(correlationId, body.getConnectionId(), body.getDataPlaneStatus(), body.getTimeStamp())))) _

  private def messageFactories[T <: NsiMessage](factories: Map[String, NsiRequestMessageFactory[T]])(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory[T]] =
    factories.get(bodyNode.getLocalName()).toRight(s"unknown body element type '${bodyNode.getLocalName}'")

  def bodyNameToProviderOperation(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory[NsiProviderOperation]] = MessageProviderFactories(bodyNode)

  def bodyNameToRequesterOperation(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory[NsiRequesterOperation]] = MessageRequesterFactories(bodyNode)

  private def onlyChildElementWithNamespace(namespaceUri: String, elem: SOAPElement) =
    elem.getChildElements().asScala.collect {
      case e: org.w3c.dom.Element if e.getNamespaceURI() == namespaceUri => e
    }.toList match {
      case Nil      => Left(s"missing NSI element in '${elem.getLocalName}', expected one")
      case e :: Nil => Right(e)
      case _        => Left(s"multiple NSI elements in '${elem.getLocalName}', expected one")
    }

  def nsiMessageToSoapMessage[T <: NsiMessage: ToXmlDocument](message: T): SOAPMessage = {
    val soap = MessageFactory.newInstance().createMessage()

    val header = SOAPFactory.newInstance().createElement(ToXmlDocument[NsiHeaders].asDocument(message.headers).getDocumentElement())

    soap.getSOAPBody().addDocument(ToXmlDocument[T].asDocument(message))
    soap.getSOAPHeader().addChildElement(header)
    soap.saveChanges()

    soap
  }

  def injectionToFormat[A, B: Format](injection: Injection[A, B]): Format[A] = new Format[A] {
    override def reads(js: JsValue): JsResult[A] = Json.fromJson[B](js).flatMap { b =>
      injection.invert(b).map(JsSuccess(_)).getOrElse(JsError("bad"))
    }
    override def writes(a: A): JsValue = Json.toJson(injection(a))
  }

  implicit val NsiProviderOperationToSoapMessage = Injection.build[NsiProviderOperation, SOAPMessage] { operation =>
    nsiMessageToSoapMessage(operation)
  } { soapToNsiMessage(bodyNameToProviderOperation)(_).right.toOption }

  implicit val NsiRequesterOperationToSoapMessage = Injection.build[NsiRequesterOperation, SOAPMessage] { operation =>
    nsiMessageToSoapMessage(operation)
  } { soapToNsiMessage(bodyNameToRequesterOperation)(_).right.toOption }

  implicit val SoapMessageToString = Injection.build[SOAPMessage, String] { soap =>
    new ByteArrayOutputStream().tap(soap.writeTo).toString("UTF-8")
  } { string =>
    Try {
      MessageFactory.newInstance().createMessage(new MimeHeaders, new ByteArrayInputStream(string.getBytes(Codec.UTF8.charSet)))
    }.toOption
  }

  implicit val NsiProviderOperationFormat: Format[NsiProviderOperation] = injectionToFormat(NsiProviderOperationToSoapMessage.andThen(SoapMessageToString))
  implicit val NsiRequesterOperationFormat: Format[NsiRequesterOperation] = injectionToFormat(NsiRequesterOperationToSoapMessage.andThen(SoapMessageToString))

  import PceMessage.ProviderEndPointFormat

  implicit val FromRequesterFormat = Json.format[FromRequester]
  implicit val ToRequesterFormat = Json.format[ToRequester]
  implicit val FromProviderFormat = Json.format[FromProvider]
  implicit val ToProviderFormat = Json.format[ToProvider]
  implicit val FromPceFormat = Json.format[FromPce]
  implicit val ToPceFormat = Json.format[ToPce]

  implicit val MessageToStoredMessage = Injection.build[Message, StoredMessage] {
    case message @ FromRequester(nsi) => StoredMessage(nsi.headers.correlationId, "NSIv2", "FromRequester", Json.toJson(message).toString)
    case message @ ToRequester(nsi)   => StoredMessage(nsi.headers.correlationId, "NSIv2", "ToRequester", Json.toJson(message).toString)
    case message @ FromProvider(nsi)  => StoredMessage(nsi.headers.correlationId, "NSIv2", "FromProvider", Json.toJson(message).toString)
    case message @ ToProvider(nsi, _) => StoredMessage(nsi.headers.correlationId, "NSIv2", "ToProvider", Json.toJson(message).toString)
    case message @ FromPce(pce)       => StoredMessage(pce.correlationId, "PCEv1", "FromPce", Json.toJson(message).toString)
    case message @ ToPce(pce)         => StoredMessage(pce.correlationId, "PCEv1", "ToPce", Json.toJson(message).toString)
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

  private def parseJson[T](json: String)(implicit reads: Reads[T]): Option[T] = Json.parse(json).validate[T].asOpt

}

class MessageStore[T]()(implicit writer: Injection[T, StoredMessage]) {
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
    val stored = writer(message)
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

  def loadAll(aggregatedConnectionId: ConnectionId): Seq[T] = DB.withConnection { implicit connection =>
    SQL("""
        SELECT correlation_id, protocol, type, content, created_at
          FROM messages
         WHERE aggregated_connection_id = {aggregated_connection_id}
         ORDER BY id ASC""").on(
      'aggregated_connection_id -> aggregatedConnectionId).as(messageParser.*).flatMap(writer.invert) // FIXME error handling
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
            case _ ~ message => writer.invert(message)
          }
      }(collection.breakOut)

  }

  private def messageParser =
    (get[UUID]("correlation_id") ~ str("protocol") ~ str("type") ~ str("content") ~ get[java.util.Date]("created_at")).map {
      case correlationId ~ protocol ~ tpe ~ content ~ createdAt => StoredMessage(CorrelationId.fromUuid(correlationId), protocol, tpe, content, new Instant(createdAt.getTime()))
    }
}
