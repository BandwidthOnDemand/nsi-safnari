package controllers

import com.ning.http.client.Realm.AuthScheme
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.w3c.dom.Document
import play.api.Logger
import play.api.http.Status._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.WS
import scala.concurrent.Future
import scala.language.higherKinds
import support.ExtraBodyParsers._

object NsiWebService {
  implicit class SoapRequestHolder(request: WS.WSRequestHolder) {
    def withSoapActionHeader(action: String) = request.withHeaders("SOAPAction" -> ('"' + action +'"'))
  }

  def callProvider(provider: ProviderEndPoint, message: NsiProviderMessage[NsiProviderOperation]): Future[NsiProviderMessage[NsiAcknowledgement]] =
    call[NsiProviderOperation, NsiProviderMessage](
      provider,
      message.body.soapActionUrl,
      message,
      (defaultHeaders, document) => NsiProviderMessageToDocument[NsiAcknowledgement](Some(defaultHeaders)).invert(document),
      (headers, exception) => NsiProviderMessage(headers, ServiceException(exception)))(NsiProviderMessageToDocument(defaultHeaders = None))

  def callRequester(provider: ProviderEndPoint, message: NsiRequesterMessage[NsiRequesterOperation]): Future[NsiRequesterMessage[NsiAcknowledgement]] =
    call[NsiRequesterOperation, NsiRequesterMessage](
      provider,
      message.body.soapActionUrl,
      message,
      (defaultHeaders, document) => NsiRequesterMessageToDocument[NsiAcknowledgement](Some(defaultHeaders)).invert(document),
      (headers, exception) => NsiRequesterMessage(headers, ServiceException(exception)))(NsiRequesterMessageToDocument(defaultHeaders = None))

  private def call[T, M[_] <: NsiMessage[_]](
    provider: ProviderEndPoint,
    soapAction: String,
    message: M[T],
    convertAck: (NsiHeaders, Document) => Either[String, M[NsiAcknowledgement]],
    convertError: (NsiHeaders, ServiceExceptionType) => M[NsiAcknowledgement])(implicit messageConversion: Conversion[M[T], Document]): Future[M[NsiAcknowledgement]] = {

    var request = WS.url(provider.url.toASCIIString())

    request = provider.authentication match {
      case OAuthAuthentication(token)              => request.withHeaders("Authorization" -> s"bearer $token")
      case BasicAuthentication(username, password) => request.withAuth(username, password, AuthScheme.BASIC)
      case _                                       => request
    }

    request = request.withSoapActionHeader(soapAction)

    Logger.debug(s"Sending provider ${provider.nsa} at ${request.url} the SOAP message: ${Conversion[M[T], Document].andThen(Conversion[Document, String]).apply(message)}")

    val defaultAckHeaders = message.headers.forSyncAck

    request.post(message).map { ack =>
      ack.status match {
        case OK | CREATED | ACCEPTED | INTERNAL_SERVER_ERROR =>
          Logger.debug(s"Parsing SOAP ack (${ack.status}) from ${provider.nsa} at ${provider.url}: ${ack.body}")

          DocumentToString.invert(ack.body).right.flatMap { document =>
            convertAck(defaultAckHeaders, document)
          } match {
            case Left(error) =>
              Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: $error")
              convertError(defaultAckHeaders, NsiError.ChildError.copy(text = error).toServiceException(provider.nsa))
            case Right(ack) =>
              Logger.debug(s"Received ack from ${provider.nsa} at ${provider.url}: $ack")
              ack
          }
        case FORBIDDEN =>
          Logger.warn(s"Authentication failed (${ack.status}) from ${provider.nsa} at ${provider.url}: ${ack.body}")
          convertError(defaultAckHeaders, NsiError.AuthenticationFailure.toServiceException(provider.nsa))
        case _ =>
          Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: ${ack.status} ${ack.statusText} ${ack.header("content-type")}\n\t${ack.body}")
          convertError(defaultAckHeaders, NsiError.ChildError.copy(text = s"Communication error: ${ack.status} ${ack.statusText}").toServiceException(provider.nsa))
      }
    }
  }
}
