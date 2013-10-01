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
import scala.language.higherKinds

object ExtraBodyParsers {
  implicit def NsiMessageContentType[T <: NsiMessage[_]](implicit conversion: Conversion[T, Document]): ContentTypeOf[T] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit def NsiMessageWriteable[T <: NsiMessage[_]](implicit conversion: Conversion[T, Document]): Writeable[T] = Writeable { message =>
    conversion.andThen(NsiXmlDocumentConversion)(message).fold({ error =>
      // Exceptions from writeable are swallowed by Play, so log these here.
      Logger.error(error)
      throw new java.io.IOException(error)
    }, bytes => bytes)
  }

  def NsiProviderEndPoint(action: NsiProviderMessage[NsiProviderOperation] => Future[NsiProviderMessage[NsiAcknowledgement]]): Action[NsiProviderMessage[NsiProviderOperation]] =
    NsiEndPoint(nsiProviderOperation)(action)

  def NsiRequesterEndPoint(action: NsiRequesterMessage[NsiRequesterOperation] => Future[NsiRequesterMessage[NsiAcknowledgement]]): Action[NsiRequesterMessage[NsiRequesterOperation]] =
    NsiEndPoint(nsiRequesterOperation)(action)

  def NsiEndPoint[M, T[_] <: NsiMessage[_]](parser: BodyParser[T[M]])(action: T[M] => Future[T[NsiAcknowledgement]])(implicit conversion: Conversion[T[NsiAcknowledgement], Document]) = Action.async(parser) { request =>
    action(request.body).map(Results.Ok(_))
  }

  def soap(parser: Conversion[Document, Array[Byte]], maxLength: Int = BodyParsers.parse.DEFAULT_MAX_TEXT_LENGTH): BodyParser[Document] = when(
    predicate = _.contentType.exists(_ == SOAPConstants.SOAP_1_1_CONTENT_TYPE),
    parser = tolerantSoap(parser: Conversion[Document, Array[Byte]], maxLength),
    badResult = _ => Future.successful(Results.UnsupportedMediaType("Expecting Content-Type " + SOAPConstants.SOAP_1_1_CONTENT_TYPE)))

  def tolerantSoap(parser: Conversion[Document, Array[Byte]], maxLength: Int): BodyParser[Document] = BodyParser("SOAP, maxLength=" + maxLength) { request =>
    import scala.language.reflectiveCalls
    Traversable.takeUpTo[Array[Byte]](maxLength)
      .apply(
        Iteratee.consume[Array[Byte]]().map { bytes =>
          Logger.debug(s"received SOAP message ${request.uri} from ${request.remoteAddress} with content-type ${request.contentType} ${new String(bytes, "UTF-8")}")
          bytes
        }.map { bytes =>
          parser.invert.apply(bytes)
        }.map { parsed =>
          Logger.debug(s"SOAP message parse result ${parsed}")
          parsed
        }).flatMap(Iteratee.eofOrElse(Results.EntityTooLarge))
      .flatMap {
        case Left(b) =>
          Done(Left(b), Empty)
        case Right(it) =>
          it.flatMap {
            case Left(error) =>
              Logger.warn(s"SOAP parsing failed ${request.uri} from ${request.remoteAddress} with content-type ${request.contentType}: $error")
              Done(Left(Results.BadRequest(
                <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
                  <S:Body>
                    <S:Fault xmlns:ns4="http://www.w3.org/2003/05/soap-envelope">
                      <faultcode>S:Client</faultcode>
                      <faultstring>Error parsing SOAP request: { error }</faultstring>
                    </S:Fault>
                  </S:Body>
                </S:Envelope>).as(SOAPConstants.SOAP_1_1_CONTENT_TYPE)), Empty)
            case Right(xml) =>
              Done(Right(xml), Empty)
          }
      }
  }

  private[support] def nsiProviderOperation = nsiBodyParser[NsiProviderMessage[NsiProviderOperation]]

  private[support] def nsiRequesterOperation = nsiBodyParser[NsiRequesterMessage[NsiRequesterOperation]]

  private def nsiBodyParser[T <: NsiMessage[_]](implicit conversion: Conversion[T, Document]): BodyParser[T] = soap(NsiXmlDocumentConversion).flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val parsedMessage = conversion.invert(soapMessage)

      Logger.debug(s"Received (${requestHeader.uri}): $parsedMessage")

      Done(parsedMessage.left.map { error =>
        Logger.warn(s"Failed to parse $soapMessage with $error on ${requestHeader.uri}")
        Results.BadRequest(
          <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
            <S:Body>
              <S:Fault xmlns:ns4="http://www.w3.org/2003/05/soap-envelope">
                <faultcode>S:Client</faultcode>
                <faultstring>Error parsing NSI message in SOAP request: { error }</faultstring>
              </S:Fault>
            </S:Body>
          </S:Envelope>).as(SOAPConstants.SOAP_1_1_CONTENT_TYPE)
      })
    }
  }
}
