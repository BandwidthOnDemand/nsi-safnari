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
import nl.surfnet.nsi._
import scala.reflect.ClassTag
import scala.concurrent.Future
import play.api.mvc.AsyncResult
import scala.concurrent.ExecutionContext

object ExtraBodyParsers {
  import ExecutionContext.Implicits.global
  private val logger = Logger("ExtraBodyParsers")

  implicit val NsiMessageContentType: ContentTypeOf[NsiMessage] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit val NsiMessageWriteable: Writeable[NsiMessage] = Writeable { message =>
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

  def NsiEndPoint(action: NsiProviderOperation => Future[NsiResponseMessage]): Action[NsiProviderOperation] = Action(nsiRequestMessage) { request =>
    AsyncResult { action(request.body).map(Results.Ok(_)) }
  }

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

  trait NsiRequestMessageFactory {
    type JaxbMessage
    def klass: Class[JaxbMessage]
    def apply(headers: NsiHeaders, body: JaxbMessage): NsiProviderOperation
  }
  def NsiRequestMessageFactory[M](f: (NsiHeaders, M) => NsiProviderOperation)(implicit manifest: ClassTag[M]) = new NsiRequestMessageFactory {
    override type JaxbMessage = M
    override def klass = manifest.runtimeClass.asInstanceOf[Class[M]]
    override def apply(headers: NsiHeaders, body: M): NsiProviderOperation = f(headers, body)
  }

  def nsiRequestMessage(): BodyParser[NsiProviderOperation] = soap.flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val unmarshaller = NsiMessage.unmarshaller

      val parsedMessage = for {
        headerNode <- onlyChildElementWithNamespace(NsiFrameworkHeaderNamespace, soapMessage.getSOAPHeader()).right
        bodyNode <- onlyChildElementWithNamespace(NsiConnectionTypesNamespace, soapMessage.getSOAPBody()).right
        messageFactory <- bodyNameToClass(bodyNode).right
        header <- tryEither(unmarshaller.unmarshal(headerNode, classOf[CommonHeaderType]).getValue).right
        body <- tryEither(unmarshaller.unmarshal(bodyNode, messageFactory.klass).getValue).right
        correlationId <- tryEither(UUID.fromString(header.getCorrelationId().drop(9))).right
        replyTo <- tryEither(Option(header.getReplyTo()).map(URI.create)).right
      } yield {
        messageFactory(NsiHeaders(correlationId, replyTo), body)
      }

      Done(parsedMessage.left.map(Results.BadRequest(_)))
    }
  }

  private def tryEither[A](f: => A): Either[String, A] = Try(f).toEither.left.map(_.toString)

  private val MessageFactories = Map(
    "reserve" -> NsiRequestMessageFactory[ReserveType]((headers, _) => NsiProviderOperation.Reserve(headers)),
    "querySummary" -> NsiRequestMessageFactory[QueryType]((headers, body) => NsiProviderOperation.QuerySummary(headers, body.getConnectionId().asScala)))

  private def bodyNameToClass(bodyNode: org.w3c.dom.Element): Either[String, NsiRequestMessageFactory] =
    MessageFactories.get(bodyNode.getLocalName()).toRight(s"unknown body element type '${bodyNode.getLocalName}'")

  private def onlyChildElementWithNamespace(namespaceUri: String, elem: SOAPElement) =
    elem.getChildElements().asScala.collect {
      case e: org.w3c.dom.Element if e.getNamespaceURI() == namespaceUri => e
    }.toList match {
      case Nil      => Left(s"missing NSI element in '${elem.getLocalName}', expected one")
      case e :: Nil => Right(e)
      case _        => Left(s"multiple NSI elements in '${elem.getLocalName}', expected one")
    }
}
