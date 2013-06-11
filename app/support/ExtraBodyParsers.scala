package support

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import javax.xml.bind.{ JAXBContext, Unmarshaller }
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
import org.w3c.dom.Document
import javax.xml.soap.SOAPConstants

object ExtraBodyParsers {
  private val logger = Logger("ExtraBodyParsers")

  implicit def NsiMessageContentType[T <: NsiMessage](implicit conversion: Conversion[T, Document]): ContentTypeOf[T] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit def NsiMessageWriteable[T <: NsiMessage](implicit conversion: Conversion[T, Document]): Writeable[T] = Writeable { message =>
    conversion.andThen(NsiXmlDocumentConversion)(message).fold({ error =>
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

  def soap(parser: Conversion[Document, Array[Byte]], maxLength: Int = BodyParsers.parse.DEFAULT_MAX_TEXT_LENGTH): BodyParser[Document] = when(
    _.contentType.exists(_ == SOAPConstants.SOAP_1_1_CONTENT_TYPE),
    tolerantSoap(parser: Conversion[Document, Array[Byte]], maxLength),
    _ => Results.BadRequest("Expecting " + SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  def tolerantSoap(parser: Conversion[Document, Array[Byte]], maxLength: Int): BodyParser[Document] = BodyParser("SOAP, maxLength=" + maxLength) { request =>
    import scala.language.reflectiveCalls
    Traversable.takeUpTo[Array[Byte]](maxLength)
      .apply(Iteratee.consume[Array[Byte]]().mapDone(parser.invert.apply))
      .flatMap(Iteratee.eofOrElse(Results.EntityTooLarge))
      .flatMap {
        case Left(b) => Done(Left(b), Empty)
        case Right(it) => it.flatMap {
          case Left(error) => Done(Left(Results.BadRequest(error)), Empty)
          case Right(xml)  => Done(Right(xml), Empty)
        }
      }
  }

  private[support] def nsiProviderOperation = nsiBodyParser[NsiProviderOperation]

  private[support] def nsiRequesterOperation = nsiBodyParser[NsiRequesterOperation]

  private def nsiBodyParser[T <: NsiMessage](implicit conversion: Conversion[T, Document]): BodyParser[T] = soap(NsiXmlDocumentConversion).flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val parsedMessage = conversion.invert(soapMessage)

      Done(parsedMessage.left.map { error =>
        Logger.warn(s"Failed to parse $soapMessage with $error")
        Results.BadRequest(error)
      })
    }
  }
}
