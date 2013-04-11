package controllers

import play.api.mvc._
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import support.ExtraBodyParsers._
import models.NsiProviderOperation
import models.NsiResponseMessage
import java.util.UUID
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.WS
import models.NsiRequesterOperation._
import models.NsiRequesterOperation

object ConnectionProvider extends Controller with SoapWebService {

  private val state = scala.concurrent.stm.Ref(Map.empty[String, String])

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionProvider.request().absoluteURL()

  def request = NsiEndPoint(handleMessage)

  private[controllers] def handleMessage(message: NsiProviderOperation): NsiResponseMessage = message match {
    case r: NsiProviderOperation.Reserve =>
      val connectionId = UUID.randomUUID.toString()
      state.single.transform(_ + (connectionId -> "foo"))
      reply(r, ReserveFailed(r.headers.copy(replyTo = None), connectionId))
      NsiResponseMessage.ReserveResponse(r.headers, connectionId)
    case q: NsiProviderOperation.QuerySummary =>
      val connectionStates = state.single.getWith(state => q.connectionIds.map(id => state.get(id).map(id -> _))).flatten
      reply(q, NsiRequesterOperation.QuerySummaryConfirmed(q.headers.copy(replyTo = None), connectionStates.map(_._1)))
      NsiResponseMessage.GenericAck(q.headers)
    case m =>
      NsiResponseMessage.ServiceException(m.headers)
  }

  private def reply(request: NsiProviderOperation, response: NsiRequesterOperation): Unit = request.replyTo.foreach { replyTo =>
    Future {
      blocking { Thread.sleep(3000) }
      WS.url(replyTo.toASCIIString()).post(response)
    }
  }
}
