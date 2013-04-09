package controllers

import scala.util.Try
import scala.collection.JavaConverters._
import javax.xml.bind.JAXBContext
import play.api.mvc._
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import support.ExtraBodyParsers._
import models.NsiRequestMessage
import models.NsiResponseMessage
import play.api.libs.iteratee.Enumerator
import scalax.io._
import play.api.http.ContentTypes

object ConnectionProvider extends Controller {

  private val WsdlResourcePath = "wsdl/2.0"
  private val WsdlFilenamePattern = "[a-z0-9_-]+(\\.[a-z0-9_-]+)*\\.(wsdl|xsd)"
  private val BaseWsdlFilename = "ogf_nsi_connection_provider_v2_0.wsdl"
  require(BaseWsdlFilename matches WsdlFilenamePattern, "base WSDL filename does not match WSDL filename pattern")

  private def readClasspathResource(resource: String): Option[String] =
    Try(Resource.fromClasspath(resource).string(scalax.io.Codec.UTF8)).toOption

  private def readClasspathWsdl(name: String): Option[String] =
    Some(name).filter(_ matches WsdlFilenamePattern).flatMap(name => readClasspathResource(s"$WsdlResourcePath/$name"))

  private def replaceSoapAddress(wsdl: String)(implicit request: RequestHeader) = {
    val serviceUrl = routes.ConnectionProvider.request().absoluteURL()
    wsdl.replaceAll(
      """(?i)<\s*soap:address\s+location\s*=\s*['"](.*)['"]\s*/>""",
      s"""<soap:address location="$serviceUrl" />""")
  }

  def wsdl(wsdl: String) = wsdlOrXsd(BaseWsdlFilename)

  def wsdlOrXsd(name: String) = Action { implicit request =>
    readClasspathWsdl(name).map {
      name match {
        case BaseWsdlFilename => replaceSoapAddress
        case _                => identity
      }
    }.map { body =>
      Ok(body).as(ContentTypes.XML)
    }.getOrElse {
      NotFound(s"Resource '$name' not found")
    }
  }

  def request = NsiEndPoint {
    case r: NsiRequestMessage.Reserve =>
      NsiResponseMessage.GenericAck(r.correlationId)
    case q: NsiRequestMessage.QuerySummary =>
      NsiResponseMessage.GenericAck(q.correlationId)
    case _ =>
      NsiResponseMessage.GenericFail()
  }

}
