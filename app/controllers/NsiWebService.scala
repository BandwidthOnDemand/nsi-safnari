/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package controllers

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.soap.ExtraBodyParsers._
import nl.surfnet.nsiv2.soap.NsiSoapConversions._
import nl.surfnet.nsiv2.soap._
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.w3c.dom.Document
import play.api.Logger
import play.api.Play.current
import play.api.http.Status._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.{WS, WSRequestHolder}
import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import java.net.URI

object NsiWebService {
  implicit class SoapRequestHolder(request: WSRequestHolder) {
    def withSoapActionHeader(action: String) = request.withHeaders("SOAPAction" -> ('"' + action +'"'))
  }

  def callProvider(provider: ProviderEndPoint, message: NsiProviderMessage[NsiProviderOperation]): Future[NsiProviderMessage[NsiAcknowledgement]] =
    call[NsiProviderOperation, NsiProviderMessage](
      provider.nsa,
      provider.url,
      message.body.soapActionUrl,
      message,
      (defaultHeaders, document) => NsiProviderMessageToDocument[NsiAcknowledgement](Some(defaultHeaders)).invert(document),
      (headers, exception) => NsiProviderMessage(headers, ServiceException(exception)))(NsiProviderMessageToDocument(defaultHeaders = None))

  def callRequester(requesterNsa: String, requesterUri: URI, message: NsiRequesterMessage[NsiRequesterOperation]): Future[NsiRequesterMessage[NsiAcknowledgement]] =
    call[NsiRequesterOperation, NsiRequesterMessage](
      requesterNsa,
      requesterUri,
      message.body.soapActionUrl,
      message,
      (defaultHeaders, document) => NsiRequesterMessageToDocument[NsiAcknowledgement](Some(defaultHeaders)).invert(document),
      (headers, exception) => NsiRequesterMessage(headers, ServiceException(exception)))(NsiRequesterMessageToDocument(defaultHeaders = None))

  private def call[T <: NsiOperation, M[_ <: NsiOperation] <: NsiMessage[_]](
    nsa: String,
    url: URI,
    soapAction: String,
    message: M[T],
    convertAck: (NsiHeaders, Document) => Try[M[NsiAcknowledgement]],
    convertError: (NsiHeaders, ServiceExceptionType) => M[NsiAcknowledgement])(implicit messageConversion: Conversion[M[T], Document]): Future[M[NsiAcknowledgement]] = {

    for {
      providerUrl <- if (Configuration.Use2WayTLS) Future.fromTry(Configuration.translateToStunnelAddress(nsa, url)) else Future.successful(url)
      request = WS.url(providerUrl.toASCIIString()).withRequestTimeout(20000).withSoapActionHeader(soapAction)
      _ = Logger.debug(s"Sending NSA ${nsa} at ${request.url} the SOAP message: ${Conversion[M[T], Document].andThen(Conversion[Document, String]).apply(message)}")
      ack <- request.post(message)
    } yield {
      val defaultAckHeaders = message.headers.forSyncAck

      ack.status match {
        case OK | CREATED | ACCEPTED | INTERNAL_SERVER_ERROR =>
          Logger.debug(s"Parsing SOAP ack (${ack.status}) from ${nsa} at ${url}: ${ack.body}")

          DocumentToString.invert(ack.body).flatMap { document =>
            convertAck(defaultAckHeaders, document)
          } match {
            case Failure(error) =>
              Logger.warn(s"Communication error with provider ${nsa} at ${url}: $error", error)
              convertError(defaultAckHeaders, NsiError.ChildError.copy(text = error.toString).toServiceException(nsa))
            case Success(ack) =>
              Logger.debug(s"Received ack from ${nsa} at ${url}: $ack")
              ack
          }
        case FORBIDDEN =>
          Logger.warn(s"Authentication failed (${ack.status}) from ${nsa} at ${url}: ${ack.body}")
          convertError(defaultAckHeaders, NsiError.AuthenticationFailure.toServiceException(nsa))
        case _ =>
          Logger.warn(s"Communication error with provider ${nsa} at ${url}: ${ack.status} ${ack.statusText} ${ack.header("content-type")}\n\t${ack.body}")
          convertError(defaultAckHeaders, NsiError.ChildError.copy(text = s"Communication error: ${ack.status} ${ack.statusText}").toServiceException(nsa))
      }
    }
  }
}
