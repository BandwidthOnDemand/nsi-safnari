package nl.surfnet

import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.safnari.{ Conversion, CorrelationId, Uuid }
import org.ogf.schemas.nsi._2013._07.connection.types.{ ReservationConfirmCriteriaType, ReservationRequestCriteriaType }
import org.ogf.schemas.nsi._2013._07.framework.types.TypeValuePairListType
import scala.util.{ Failure, Success, Try }
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import javax.xml.bind.JAXBElement
import collection.JavaConverters._
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetVlanType
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetBaseType
import javax.xml.namespace.QName

package object safnari {
  type ConnectionId = String

  private val UuidGenerator = Uuid.randomUuidGenerator

  def newConnectionId: ConnectionId = UuidGenerator().toString
  def newCorrelationId: CorrelationId = CorrelationId.fromUuid(UuidGenerator())

  implicit class AnyOps[A](a: A) {
    def tap(f: A => Unit): A = { f(a); a }
    def pp: A = { Console.err.println(a); a }
    def pp(prefix: String): A = { Console.err.println(s"$prefix: $a"); a }
  }

  def classpathResourceUri(name: String): URI = {
    val classLoader = Thread.currentThread().getContextClassLoader()
    val resource = classLoader.getResource(name)
    if (resource != null) resource.toURI()
    else throw new IllegalArgumentException(f"classpath resource '$name' not found")
  }

  implicit class TryOps[A](a: Try[A]) {
    def toEither: Either[Throwable, A] = a match {
      case Failure(t) => Left(t)
      case Success(a) => Right(a)
    }
  }

  def tryEither[A](f: => A): Either[String, A] = Try(f).toEither.left.map(_.toString)

  implicit object XmlGregorianCalendarOrdering extends Ordering[XMLGregorianCalendar] {
    def compare(x: XMLGregorianCalendar, y: XMLGregorianCalendar): Int = x compare y
  }

  implicit val ReservationCriteriaConversion = Conversion.build[ReservationConfirmCriteriaType, ReservationRequestCriteriaType] { a =>
    Right(new ReservationRequestCriteriaType().
      withSchedule(a.getSchedule()).
      withAny(a.getAny()).
      withServiceType(a.getServiceType()).
      withVersion(a.getVersion()).
      tap(_.getOtherAttributes().putAll(a.getOtherAttributes())))
  } { b =>
    (for {
      schedule <- Option(b.getSchedule())
    } yield {
      new ReservationConfirmCriteriaType().
        withSchedule(schedule).
        withAny(b.getAny()).
        withServiceType(b.getServiceType()).
        withVersion(if (b.getVersion() == null) 0 else b.getVersion()).
        tap(_.getOtherAttributes().putAll(b.getOtherAttributes()))
    }).toRight("cannot convert request criteria to confirm criteria. Missing fields?")
  }

  private val PointToPointObjectFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()
  private val P2PS_QNAME = PointToPointObjectFactory.createP2Ps(null).getName()
  private val ETS_QNAME = PointToPointObjectFactory.createEts(null).getName()
  private val EVTS_QNAME = PointToPointObjectFactory.createEvts(null).getName()

  private object JaxbElement {
    def unapply[A](element: JAXBElement[A]): Option[(QName, A)] = Some((element.getName(), element.getValue()))
  }
  implicit class HasAnyServiceOps[A: HasAnyService](a: A) {
    def withP2Ps(service: P2PServiceBaseType): A = {
      val element = service match {
        case evts: EthernetVlanType   => PointToPointObjectFactory.createEvts(evts)
        case ets: EthernetBaseType    => PointToPointObjectFactory.createEts(ets)
        case p2ps: P2PServiceBaseType => PointToPointObjectFactory.createP2Ps(p2ps)
      }

      // FIXME replace if already exists?
      implicitly[HasAnyService[A]].withAny(a, Seq(element))

      a
    }

    def getP2Ps(): Option[P2PServiceBaseType] = implicitly[HasAnyService[A]].getAny(a).collectFirst {
      case JaxbElement(EVTS_QNAME, evts: EthernetVlanType)   => evts
      case JaxbElement(ETS_QNAME, ets: EthernetBaseType)     => ets
      case JaxbElement(P2PS_QNAME, p2ps: P2PServiceBaseType) => p2ps
    }
  }
}
