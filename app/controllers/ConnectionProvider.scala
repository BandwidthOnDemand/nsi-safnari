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

object ConnectionProvider extends Controller with SoapWebService {

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionProvider.request().absoluteURL()

  def request = NsiEndPoint(handleMessage)

  private[controllers] def handleMessage(message: NsiProviderOperation): NsiResponseMessage = message match {
    case r: NsiProviderOperation.Reserve =>
      r.replyTo.foreach { replyTo =>
        println(s"Replying to $replyTo")
//        Future {
          blocking {
//            Thread.sleep(3000)
            WS.url(replyTo.toASCIIString()).post(ReserveFailed(r.headers.copy(replyTo = None)))
//          }
        }
      }
      NsiResponseMessage.ReserveResponse(r.headers, UUID.randomUUID.toString())
    case q: NsiProviderOperation.QuerySummary =>
      NsiResponseMessage.GenericAck(q.headers)
    case m =>
      NsiResponseMessage.ServiceException(m.headers)
  }
}
