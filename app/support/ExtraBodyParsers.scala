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
import nl.surfnet.safnari.NsiSoapConversions._
import scala.reflect.ClassTag
import scala.concurrent.Future
import play.api.mvc.AsyncResult
import play.api.libs.concurrent.Execution.Implicits._

object ExtraBodyParsers {
  private val logger = Logger("ExtraBodyParsers")

  implicit def NsiMessageContentType[T](implicit conversion: Conversion[T, SOAPMessage]): ContentTypeOf[T] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit def NsiMessageWriteable[T](implicit conversion: Conversion[T, SOAPMessage]): Writeable[T] = Writeable { message =>
    conversion.andThen(Conversion[SOAPMessage, Array[Byte]])(message).fold({ error =>
      // Exceptions from writeable are swallowed by Play, so log these here.
      logger.error(error)
      throw new java.io.IOException(error)
    }, bytes => bytes)
  }

  def NsiProviderEndPoint(action: NsiProviderOperation => Future[NsiAcknowledgement]): Action[NsiProviderOperation] =
    NsiEndPoint(nsiProviderOperation)(action)

  def NsiRequesterEndPoint(action: NsiRequesterOperation => Future[NsiAcknowledgement]): Action[NsiRequesterOperation] =
    NsiEndPoint(nsiRequesterOperation)(action)

  def NsiEndPoint[T <: NsiMessage](parser: BodyParser[T])(action: T => Future[NsiAcknowledgement]) = Action(parser) { request =>
    Logger.debug(s"Received: ${request.body}")
    AsyncResult {
      action(request.body).map { response =>
        Logger.debug(s"Respond: $response")
        Results.Ok(response)
      }
    }
  }

  def soap: BodyParser[SOAPMessage] = soap(BodyParsers.parse.DEFAULT_MAX_TEXT_LENGTH)

  def soap(maxLength: Int): BodyParser[SOAPMessage] = when(
    _.contentType.exists(_ == SOAPConstants.SOAP_1_1_CONTENT_TYPE),
    tolerantSoap(maxLength),
    _ => Results.BadRequest("Expecting " + SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  def tolerantSoap(maxLength: Int): BodyParser[SOAPMessage] = BodyParser("SOAP, maxLength=" + maxLength) { request =>
    import scala.language.reflectiveCalls
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

  private[support] def nsiProviderOperation = nsiBodyParser[NsiProviderOperation]

  private[support] def nsiRequesterOperation = nsiBodyParser[NsiRequesterOperation]

  private def nsiBodyParser[T](implicit conversion: Conversion[T, SOAPMessage]): BodyParser[T] = soap.flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val parsedMessage = conversion.invert(soapMessage)

      Done(parsedMessage.left.map { error =>
        Logger.warn(s"Failed to parse $soapMessage with $error")
        Results.BadRequest(error)
      })
    }
  }
}
