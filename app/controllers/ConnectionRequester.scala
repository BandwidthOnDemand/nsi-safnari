package controllers

import scala.concurrent.Future

import nl.surfnet.safnari._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Controller
import support.ExtraBodyParsers.NsiRequesterEndPoint

object ConnectionRequester extends Controller with SoapWebService {

  val BaseWsdlFilename = "ogf_nsi_connection_requester_v2_0.wsdl"

  override def serviceUrl: String = s"${Application.baseUrl}${routes.ConnectionRequester.request().url}"

  def expectReplyFor(correlationId: CorrelationId): Future[NsiRequesterOperation] = continuations.register(correlationId)

  def request = NsiRequesterEndPoint {
    case notification: DataPlaneStateChange =>
      ConnectionProvider.connectionManager.findBySegment(notification.connectionId).map {
        _.fold[NsiAcknowledgement](ServiceException(notification.headers.asReply, null)) { c =>
          c ! FromProvider(notification)
          GenericAck(notification.headers.asReply)
        }
      }
    case response =>
      continuations.replyReceived(response.correlationId, response)
      Future.successful(GenericAck(response.headers.asReply))
  }

  private val continuations = new Continuations[NsiRequesterOperation]()

}
