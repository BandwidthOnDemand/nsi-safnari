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
import models.{ NsiRequestMessage, NsiResponseMessage }
import scala.util.control.NonFatal
import javax.xml.validation.SchemaFactory
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.Source
import scala.reflect.ClassTag

object ExtraBodyParsers {

  implicit val nsiMessageContentType: ContentTypeOf[NsiResponseMessage] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit val nsiMessageWriteable: Writeable[NsiResponseMessage] = Writeable { message =>
    val soap = MessageFactory.newInstance().createMessage()

    val header = SOAPFactory.newInstance().createElement(message.headerDocument.getDocumentElement())

    soap.getSOAPBody().addDocument(message.bodyDocument)
    soap.getSOAPHeader().addChildElement(header)
    soap.saveChanges()

    val out = new ByteArrayOutputStream
    soap.writeTo(out)

    out.toByteArray
  }

  def NsiEndPoint(action: NsiRequestMessage => NsiResponseMessage): Action[NsiRequestMessage] = Action(nsiRequestMessage) { request =>
    Results.Ok(action(request.body))
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
    def apply(headers: CommonHeaderType, body: JaxbMessage): NsiRequestMessage
  }
  def NsiRequestMessageFactory[M](f: (CommonHeaderType, M) => NsiRequestMessage)(implicit manifest: ClassTag[M]) = new NsiRequestMessageFactory {
    override type JaxbMessage = M
    override def klass = manifest.runtimeClass.asInstanceOf[Class[M]]
    override def apply(headers: CommonHeaderType, body: M): NsiRequestMessage = f(headers, body)
  }

  private val schema = {
    val schemas = Array("wsdl/2.0/ogf_nsi_framework_headers_v2_0.xsd", "wsdl/2.0/ogf_nsi_connection_types_v2_0.xsd")
    val sources = schemas.map(Thread.currentThread().getContextClassLoader().getResource).map(schema => new StreamSource(schema.toExternalForm()): Source)
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.newSchema(sources)
  }

  def nsiRequestMessage(): BodyParser[NsiRequestMessage] = soap.flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val unmarshaller = JAXBContext.newInstance(
        classOf[ReserveType],
        classOf[CommonHeaderType],
        classOf[QueryType]).createUnmarshaller()
      unmarshaller.setSchema(schema)

      val parsedMessage = for {
        headerNode <- onlyChildElementWithNamespace(NsiFrameworkHeaderNamespace, soapMessage.getSOAPHeader()).right
        bodyNode <- onlyChildElementWithNamespace(NsiConnectionTypesNamespace, soapMessage.getSOAPBody()).right
        messageFactory <- bodyNameToClass(bodyNode).right
        header <- tryEither(unmarshaller.unmarshal(headerNode, classOf[CommonHeaderType]).getValue).right
        body <- tryEither(unmarshaller.unmarshal(bodyNode, messageFactory.klass).getValue).right
      } yield {
        messageFactory(header, body)
      }

      Done(parsedMessage.left.map(Results.BadRequest(_)))
    }
  }

  private def tryEither[A](f: => A): Either[String, A] = try Right(f) catch {
    case NonFatal(e) => Left(e.toString)
  }

  private val MessageFactories = Map(
    "reserve" -> NsiRequestMessageFactory[ReserveType]((headers, _) => NsiRequestMessage.Reserve(headers.getCorrelationId())),
    "querySummary" -> NsiRequestMessageFactory[QueryType]((headers, _) => NsiRequestMessage.QuerySummary(headers.getCorrelationId())))

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