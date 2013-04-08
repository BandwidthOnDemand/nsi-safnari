package support

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.xml.bind.{JAXBContext, Unmarshaller}
import javax.xml.soap._
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.util.{Failure, Success, Try}
import play.api.http.{ContentTypeOf, Writeable}
import play.api.libs.iteratee._
import play.api.libs.iteratee.Input.Empty
import play.api.mvc._
import play.api.mvc.BodyParsers.parse.when
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._04.framework.headers.CommonHeaderType
import models.{NsiRequestMessage, NsiResponseMessage}

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
          case Success(xml) => Done(Right(xml), Empty)
        }
      }
  }

  def nsiRequestMessage(): BodyParser[NsiRequestMessage] = soap.map { soapMessage =>
    val unmarshaller = JAXBContext.newInstance(
      classOf[ReserveType],
      classOf[CommonHeaderType]).createUnmarshaller()

    val header = unmarshaller.unmarshal(firstElement(soapMessage.getSOAPHeader()), classOf[CommonHeaderType]).getValue
    val body = unmarshaller.unmarshal(firstElement(soapMessage.getSOAPBody()), classOf[ReserveType]).getValue

    NsiRequestMessage.Reserve(header.getCorrelationId())
  }

  private def firstElement(elem: SOAPElement) =
    elem.getChildElements().asScala.collect {
      case e: org.w3c.dom.Element => e
    }.toSeq.head

}