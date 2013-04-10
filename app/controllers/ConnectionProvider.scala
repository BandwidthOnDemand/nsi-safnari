package controllers

import play.api.mvc._
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import support.ExtraBodyParsers._
import models.NsiProviderOperation
import models.NsiResponseMessage

object ConnectionProvider extends Controller with SoapWebService {

  val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"

  override def serviceUrl(implicit request: RequestHeader): String =
    routes.ConnectionProvider.request().absoluteURL()

  def request = NsiEndPoint {
    case r: NsiProviderOperation.Reserve =>
      NsiResponseMessage.GenericAck(r.correlationId)
    case q: NsiProviderOperation.QuerySummary =>
      NsiResponseMessage.GenericAck(q.correlationId)
    case _ =>
      NsiResponseMessage.GenericFail()
  }

}
