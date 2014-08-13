package controllers

import nl.surfnet.safnari.NsiSoapConversions._
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.w3c.dom.Document
import play.api.Logger
import play.api.Play.current
import play.api.http.Status._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.{WS, WSRequestHolder}
import support.ExtraBodyParsers._

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

object NsiWebService {
  implicit class SoapRequestHolder(request: WSRequestHolder) {
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
    convertAck: (NsiHeaders, Document) => Try[M[NsiAcknowledgement]],
    convertError: (NsiHeaders, ServiceExceptionType) => M[NsiAcknowledgement])(implicit messageConversion: Conversion[M[T], Document]): Future[M[NsiAcknowledgement]] = {

    val providerUrl = if (Configuration.Use2WayTLS) Configuration.translateToStunnelAddress(provider) else provider.url

    val request = WS.url(providerUrl.toASCIIString()).withRequestTimeout(20000).withSoapActionHeader(soapAction)

    Logger.debug(s"Sending provider ${provider.nsa} at ${request.url} the SOAP message: ${Conversion[M[T], Document].andThen(Conversion[Document, String]).apply(message)}")

    val defaultAckHeaders = message.headers.forSyncAck

    request.post(message).map { ack =>
      ack.status match {
        case OK | CREATED | ACCEPTED | INTERNAL_SERVER_ERROR =>
          Logger.debug(s"Parsing SOAP ack (${ack.status}) from ${provider.nsa} at ${provider.url}: ${ack.body}")

          DocumentToString.invert(ack.body).flatMap { document =>
            convertAck(defaultAckHeaders, document)
          } match {
            case Failure(error) =>
              Logger.warn(s"Communication error with provider ${provider.nsa} at ${provider.url}: $error", error)
              convertError(defaultAckHeaders, NsiError.ChildError.copy(text = error.toString).toServiceException(provider.nsa))
            case Success(ack) =>
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
