package nl.surfnet.nsiv2

import java.net.URI
import javax.xml.bind.JAXBElement
import javax.xml.namespace.QName
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType

package object messages {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  private val PointToPointObjectFactory = new org.ogf.schemas.nsi._2013._12.services.point2point.ObjectFactory()
  private val P2PS_QNAME = PointToPointObjectFactory.createP2Ps(null).getName()

  private object JaxbElement {
    def unapply[A](element: JAXBElement[A]): Option[(QName, A)] = Some((element.getName(), element.getValue()))
  }
  implicit class XmlPointToPointServiceOps[A: HasXmlAny](a: A) {
    def withPointToPointService(service: P2PServiceBaseType): A = {
      val element = PointToPointObjectFactory.createP2Ps(service)

      // FIXME replace if already exists?
      HasXmlAny[A].setAny(a, Seq(element))

      a
    }

    def getPointToPointService(): Option[P2PServiceBaseType] = HasXmlAny[A].getAny(a).collectFirst {
      case JaxbElement(P2PS_QNAME, p2ps: P2PServiceBaseType) => p2ps
    }
  }

  private [messages] implicit class RichString(str: String) {
    def uncapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }
}
