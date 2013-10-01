package controllers

import play.api.libs.ws.WS.WSRequestHolder

object SoapRequests {

  implicit class SoapRequestHolder(request: WSRequestHolder) {
    def withSoapActionHeader(action: String) = request.withHeaders("SOAPAction" -> ('"' + action +'"'))
  }
}