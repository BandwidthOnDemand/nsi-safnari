package support

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import javax.xml.bind.{ JAXBContext, Unmarshaller }
import javax.xml.soap._
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.util.{ Failure, Success, Try }
import play.api.http.{ ContentTypeOf, Writeable }
import play.api.libs.iteratee._
import play.api.libs.iteratee.Input.Empty
import play.api.mvc._
import play.api.mvc.BodyParsers.parse.when
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import scala.util.control.NonFatal
import javax.xml.validation.SchemaFactory
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.Source
import java.util.UUID
import java.net.URI
import play.api.Logger
import scala.collection.JavaConverters._
import nl.surfnet.safnari._
import scala.reflect.ClassTag
import scala.concurrent.Future
import play.api.mvc.AsyncResult
import play.api.libs.concurrent.Execution.Implicits._

object ExtraBodyParsers {
  private val logger = Logger("ExtraBodyParsers")

  implicit def NsiMessageContentType[T <: NsiMessage]: ContentTypeOf[NsiEnvelope[T]] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit def NsiMessageWriteable[T <: NsiMessage]: Writeable[NsiEnvelope[T]] = Writeable { message =>
    try {
      val soap = MessageFactory.newInstance().createMessage()

      val header = SOAPFactory.newInstance().createElement(message.headerDocument.getDocumentElement())

      soap.getSOAPBody().addDocument(message.bodyDocument)
      soap.getSOAPHeader().addChildElement(header)
      soap.saveChanges()

      new ByteArrayOutputStream().tap(soap.writeTo).toByteArray
    } catch {
      case NonFatal(e) =>
        // Exceptions from writeable are swallowed by Play, so log these here.
        logger.error(f"error writing SOAP message: $e", e)
        throw e
    }
  }

  def NsiProviderEndPoint(action: NsiEnvelope[NsiProviderOperation] => Future[NsiAcknowledgement]): Action[NsiEnvelope[NsiProviderOperation]] =
    NsiEndPoint(nsiProviderOperation)(action)

  def NsiRequesterEndPoint(action: NsiEnvelope[NsiRequesterOperation] => Future[NsiAcknowledgement]): Action[NsiEnvelope[NsiRequesterOperation]] =
    NsiEndPoint(nsiRequesterOperation)(action)

  def NsiEndPoint[T <: NsiMessage](parser: BodyParser[NsiEnvelope[T]])(action: NsiEnvelope[T] => Future[NsiAcknowledgement]) = Action(parser) { request =>
    AsyncResult { action(request.body).map(response => Results.Ok(NsiEnvelope(request.body.headers.asReply, response))) }
  }

  def nsiProviderOperation = nsiBodyParser(bodyNameToProviderOperation)

  def nsiRequesterOperation = nsiBodyParser(bodyNameToRequesterOperation)

  def soap: BodyParser[SOAPMessage] = soap(BodyParsers.parse.DEFAULT_MAX_TEXT_LENGTH)

  def soap(maxLength: Int): BodyParser[SOAPMessage] = when(
    _.contentType.exists(_ == SOAPConstants.SOAP_1_1_CONTENT_TYPE),
    tolerantSoap(maxLength),
    _ => Results.BadRequest("Expecting " + SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  import scala.language.reflectiveCalls
  def tolerantSoap(maxLength: Int): BodyParser[SOAPMessage] = BodyParser("SOAP, maxLength=" + maxLength) { request =>
    Traversable.takeUpTo[Array[Byte]](maxLength).apply(Iteratee.consume[Array[Byte]]().mapDone { bytes =>
      Try {
        val message = MessageFactory.newInstance().createMessage(new MimeHeaders, new ByteArrayInputStream(bytes))
        message.getSOAPBody() // Force parsing of the SOAP message, may throw SOAP exception.
        message
      }
    }).flatMap(Iteratee.eofOrElse(Results.EntityTooLarge))
      .flatMap {
        case Left(b) => Done(Left(b), Empty)
        case Right(it) => it.flatMap {
          case Failure(exception) => Done(Left(Results.BadRequest), Empty)
          case Success(xml)       => Done(Right(xml), Empty)
        }
      }
  }

  private val NsiFrameworkHeaderNamespace = "http://schemas.ogf.org/nsi/2013/04/framework/headers"
  private val NsiConnectionTypesNamespace = "http://schemas.ogf.org/nsi/2013/04/connection/types"

  trait NsiRequestMessageFactory[T <: NsiMessage] {
    type JaxbMessage
    def klass: Class[JaxbMessage]
    def apply(headers: NsiHeaders, body: JaxbMessage): NsiEnvelope[T]
  }

  def NsiRequestMessageFactory[M, T <: NsiMessage](f: (CorrelationId, M) => T)(implicit manifest: ClassTag[M]) = new NsiRequestMessageFactory[T] {
    override type JaxbMessage = M
    override def klass = manifest.runtimeClass.asInstanceOf[Class[M]]
    override def apply(headers: NsiHeaders, body: M): NsiEnvelope[T] = NsiEnvelope(headers, f(headers.correlationId, body))
  }

  def nsiBodyParser[T <: NsiMessage](elementToFactory: org.w3c.dom.Element => Either[String, NsiRequestMessageFactory[T]]): BodyParser[NsiEnvelope[T]] = soap.flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val unmarshaller = NsiMessage.unmarshaller

      val parsedMessage = for {
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

      Done(parsedMessage.left.map { error =>
        Logger.warn(s"Failed to parse $soapMessage with $error")
        Results.BadRequest(error)
      })
    }
  }

  private val MessageProviderFactories = messageFactories(Map(
    "reserve" -> NsiRequestMessageFactory[ReserveType, NsiProviderOperation](Reserve),
    "reserveCommit" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => ReserveCommit(correlationId, body.getConnectionId())),
    "reserveAbort" -> NsiRequestMessageFactory[GenericRequestType, NsiProviderOperation]((correlationId, body) => ReserveAbort(correlationId, body.getConnectionId())),
    "querySummary" -> NsiRequestMessageFactory[QueryType, NsiProviderOperation]((correlationId, body) => QuerySummary(correlationId, body.getConnectionId().asScala)),
    "querySummarySync" -> NsiRequestMessageFactory[QueryType, NsiProviderOperation]((correlationId, body) => QuerySummarySync(correlationId, body.getConnectionId().asScala)))) _

  private val MessageRequesterFactories = messageFactories(Map(
    "reserveConfirmed" -> NsiRequestMessageFactory[ReserveConfirmedType, NsiRequesterOperation]((correlationId, body) => ReserveConfirmed(correlationId, body.getConnectionId(), body.getCriteria().asScala.head)),
    "reserveCommitConfirmed" -> NsiRequestMessageFactory[GenericConfirmedType, NsiRequesterOperation]((correlationId, body) => ReserveCommitConfirmed(correlationId, body.getConnectionId)))) _

  private def messageFactories[T <: NsiMessage](factories: Map[String, NsiRequestMessageFactory[T]])(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory[T]] =
    factories.get(bodyNode.getLocalName()).toRight(s"unknown body element type '${bodyNode.getLocalName}'")

  private def bodyNameToProviderOperation(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory[NsiProviderOperation]] = MessageProviderFactories(bodyNode)

  private def bodyNameToRequesterOperation(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory[NsiRequesterOperation]] = MessageRequesterFactories(bodyNode)

  private def onlyChildElementWithNamespace(namespaceUri: String, elem: SOAPElement) =
    elem.getChildElements().asScala.collect {
      case e: org.w3c.dom.Element if e.getNamespaceURI() == namespaceUri => e
    }.toList match {
      case Nil      => Left(s"missing NSI element in '${elem.getLocalName}', expected one")
      case e :: Nil => Right(e)
      case _        => Left(s"multiple NSI elements in '${elem.getLocalName}', expected one")
    }
}
