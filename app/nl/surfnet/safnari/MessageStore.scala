package nl.surfnet.safnari

import play.api.libs.json.Format
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
import play.api.libs.json.Json
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import java.net.URI
import java.io.ByteArrayInputStream
import play.api.libs.json.Reads
import java.util.UUID

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

  val NsiMessageToStoredMessage = Injection.build[NsiMessage, StoredMessage] { message =>
    val (tpe, soap) = message match {
      case message: NsiAcknowledgement    => "NsiAcknowledgement" -> nsiMessageToSoapMessage(message)
      case message: NsiProviderOperation  => "NsiProviderOperation" -> nsiMessageToSoapMessage(message)
      case message: NsiRequesterOperation => "NsiRequesterOperation" -> nsiMessageToSoapMessage(message)
    }
    val content = new ByteArrayOutputStream().tap(soap.writeTo).toString("UTF-8")
    StoredMessage(message.headers.correlationId, "NSIv2", tpe, content)
  } { stored =>
    val soap = MessageFactory.newInstance().createMessage(new MimeHeaders, new ByteArrayInputStream(stored.content.getBytes(Codec.UTF8.charSet)))
    val parser = stored.tpe match {
      case "NsiAcknowledgement"    => ???
      case "NsiProviderOperation"  => soapToNsiMessage(bodyNameToProviderOperation)(_)
      case "NsiRequesterOperation" => soapToNsiMessage(bodyNameToRequesterOperation)(_)
    }
    parser(soap).right.toOption
  }

  val PceMessageToStoredMessage = Injection.build[PceMessage, StoredMessage] { message =>
    val (tpe, content) = message match {
      case request: PceRequest   => "PceRequest" -> Json.stringify(Json.toJson(request))
      case response: PceResponse => "PceResponse" -> Json.stringify(Json.toJson(response))
    }
    StoredMessage(message.correlationId, "PCEv1", tpe, content)
  } { stored =>
    val message = stored.tpe match {
      case "PceRequest"  => Json.fromJson[PceRequest](Json.parse(stored.content))
      case "PceResponse" => Json.fromJson[PceResponse](Json.parse(stored.content))
    }
    message.asOpt
  }

  implicit val NsiOrPceMessage = Injection.build[Either[NsiMessage, PceMessage], StoredMessage] {
    case Left(nsi)  => NsiMessageToStoredMessage(nsi)
    case Right(pce) => PceMessageToStoredMessage(pce)
  } { stored =>
    stored.protocol match {
      case "NSIv2" => NsiMessageToStoredMessage.invert(stored).map(Left(_))
      case "PCEv1" => PceMessageToStoredMessage.invert(stored).map(Right(_))
    }
  }
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
      'aggregated_connection_id -> aggregatedConnectionId).as(
        (get[UUID]("correlation_id") ~ str("protocol") ~ str("type") ~ str("content") ~ get[java.util.Date]("created_at")).*).map {
          case correlationId ~ protocol ~ tpe ~ content ~ createdAt => StoredMessage(CorrelationId.fromUuid(correlationId), protocol, tpe, content, new Instant(createdAt.getTime()))
        }.flatMap(writer.invert) // FIXME error handling
  }
}
