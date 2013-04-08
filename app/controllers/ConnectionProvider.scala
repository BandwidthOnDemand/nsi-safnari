package controllers

import scala.util.Try
import scala.collection.JavaConverters._
import javax.xml.bind.JAXBContext
import play.api.mvc._
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import support.ExtraBodyParsers._
import javax.xml.soap.SOAPMessage
import javax.xml.soap.MessageFactory
import models.NsiRequestMessage
import models.NsiResponseMessage

object ConnectionProvider extends Controller {

  def request = NsiEndPoint {
    case r: NsiRequestMessage.Reserve =>
      NsiResponseMessage.GenericAck(r.correlationId)
    case _ =>
      NsiResponseMessage.GenericFail()
  }

}
