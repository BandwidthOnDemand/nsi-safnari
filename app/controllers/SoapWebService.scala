package controllers

import play.api.http.ContentTypes
import play.api.mvc._
import play.api.mvc.Results._
import scala.util.Try
import scalax.io.Resource

trait SoapWebService {

  val BaseWsdlFilename: String

  def serviceUrl: String

  def wsdl(ignore: String) = SoapWebService.serveWsdl(BaseWsdlFilename) { implicit request =>
    replaceSoapAddress
  }

  private def replaceSoapAddress(wsdl: String) = {
    wsdl.replaceAll(
      """(?i)<\s*soap:address\s+location\s*=\s*['"](.*)['"]\s*/>""",
      s"""<soap:address location="$serviceUrl" />""")
  }

}

object SoapWebService {

  private val WsdlResourcePath = "wsdl/2.0"
  private val WsdlFilenamePattern = "[a-z0-9_-]+(\\.[a-z0-9_-]+)*\\.(wsdl|xsd)"

  def wsdlOrXsd(name: String) = serveWsdl(name){ _ => identity }

  private def serveWsdl(file: String)(transform: RequestHeader => String => String) = Action { implicit request =>
    readClasspathWsdl(file).map(transform(request)).map { body =>
      Ok(body).as(ContentTypes.XML)
    }.getOrElse {
      NotFound(s"Resource '$file' not found")
    }
  }

  private def readClasspathResource(resource: String): Option[String] =
    Try(Resource.fromClasspath(resource).string(scalax.io.Codec.UTF8)).toOption

  private def readClasspathWsdl(name: String): Option[String] =
    Some(name).filter(_ matches WsdlFilenamePattern).flatMap(name => readClasspathResource(s"$WsdlResourcePath/$name"))

}