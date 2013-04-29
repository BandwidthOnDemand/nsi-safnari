package controllers

import play.api.mvc._
import play.api.mvc.Results._
import support.ExtraBodyParsers._
import nl.surfnet.nsi.NsiResponseMessage
import nl.surfnet.nsi.NsiEnvelope
import nl.surfnet.nsi.Continuations
import nl.surfnet.nsi.CorrelationId
import nl.surfnet.nsi.NsiRequesterOperation
import scala.concurrent.Future

object ConnectionRequester extends Controller with SoapWebService {

  val BaseWsdlFilename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionRequester.request().absoluteURL()

  def expectReplyFor(correlationId: CorrelationId): Future[NsiRequesterOperation] = continuations.register(correlationId)

  def request = NsiRequesterEndPoint {
    case NsiEnvelope(headers, _) => Future.successful(NsiResponseMessage.ServiceException(headers.correlationId, "Not implemented yet"))
  }

  private val continuations = new Continuations[NsiRequesterOperation]()

}
