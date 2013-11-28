package nl.surfnet

import java.math.BigInteger
import java.net.URI
import javax.xml.bind.JAXBElement
import javax.xml.namespace.QName
import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.DatatypeConfigurationException
import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.safnari.{ Conversion, CorrelationId, Uuid }
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._07.connection.types.{ ReservationConfirmCriteriaType, ReservationRequestCriteriaType }
import org.ogf.schemas.nsi._2013._07.connection.types.ScheduleType
import org.ogf.schemas.nsi._2013._07.framework.types.TypeValuePairListType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetVlanType
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetBaseType
import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

package object safnari {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  private val UuidGenerator = Uuid.randomUuidGenerator

  def newConnectionId: ConnectionId = UuidGenerator().toString

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

  implicit class RichString(str: String) {
    def deCapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }

  def tryEither[A](f: => A): Either[String, A] = Try(f).toEither.left.map(_.toString)

  implicit object XmlGregorianCalendarOrdering extends Ordering[XMLGregorianCalendar] {
    def compare(x: XMLGregorianCalendar, y: XMLGregorianCalendar): Int = x compare y
  }
  implicit object DateTimeOrdering extends Ordering[DateTime] {
    def compare(x: DateTime, y: DateTime): Int = x compareTo y
  }

  implicit class ReadableInstantOps(instant: org.joda.time.ReadableInstant) {
    def toSqlTimestamp = new java.sql.Timestamp(instant.getMillis())
  }
  implicit class DateTimeOps(dt: org.joda.time.ReadableDateTime) {
    def toXmlGregorianCalendar = {
      DatatypeFactory.newInstance().newXMLGregorianCalendar(
        BigInteger.valueOf(dt.getYear()),
        dt.getMonthOfYear(),
        dt.getDayOfMonth(),
        dt.getHourOfDay(),
        dt.getMinuteOfHour(),
        dt.getSecondOfMinute(),
        null,
        (dt.getZone().getOffset(dt.getMillis()) / (60 * 1000)))
    }
  }
  implicit class XmlGregorianCalendarOps(dt: XMLGregorianCalendar) {
    def toDateTime = new DateTime(dt.toGregorianCalendar())
  }
  implicit class ScheduleTypeOps(schedule: ScheduleType) {
    def startTime = Option(schedule.getStartTime).map(_.toDateTime)
    def endTime = Option(schedule.getEndTime).map(_.toDateTime)
  }

  implicit class OptionEitherOps[A, B](value: scala.Option[Either[A, B]]) {
    def sequence: Either[A, scala.Option[B]] = value match {
      case None           => Right(None)
      case Some(Left(a))  => Left(a)
      case Some(Right(b)) => Right(Some(b))
    }
  }
  implicit class EitherOptionOps[A, B](value: Either[A, Option[B]]) {
    def sequence: Option[Either[A, B]] = value match {
      case Left(a)        => Some(Left(a))
      case Right(None)    => None
      case Right(Some(b)) => Some(Right(b))
    }
  }
  implicit val ReservationCriteriaConversion = Conversion.build[ReservationConfirmCriteriaType, ReservationRequestCriteriaType] { a =>
    Right(new ReservationRequestCriteriaType().
      withSchedule(a.getSchedule()).
      withAny(a.getAny()).
      withServiceType(a.getServiceType()).
      withVersion(a.getVersion()).
      tap(_.getOtherAttributes().putAll(a.getOtherAttributes())))
  } { b =>
    for {
      schedule <- Option(b.getSchedule()).toRight("schedule is required").right
      serviceType <- Option(b.getServiceType()).toRight("serviceType is required").right
    } yield {
      new ReservationConfirmCriteriaType().
        withSchedule(schedule).
        withAny(b.getAny()).
        withServiceType(b.getServiceType()).
        withVersion(if (b.getVersion() == null) 0 else b.getVersion()).
        tap(_.getOtherAttributes().putAll(b.getOtherAttributes()))
    }
  }

  private val PointToPointObjectFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()
  private val P2PS_QNAME = PointToPointObjectFactory.createP2Ps(null).getName()
  private val ETS_QNAME = PointToPointObjectFactory.createEts(null).getName()
  private val EVTS_QNAME = PointToPointObjectFactory.createEvts(null).getName()

  private object JaxbElement {
    def unapply[A](element: JAXBElement[A]): Option[(QName, A)] = Some((element.getName(), element.getValue()))
  }
  implicit class XmlPointToPointServiceOps[A: HasXmlAny](a: A) {
    def withPointToPointService(service: P2PServiceBaseType): A = {
      val element = service match {
        case evts: EthernetVlanType   => PointToPointObjectFactory.createEvts(evts)
        case ets: EthernetBaseType    => PointToPointObjectFactory.createEts(ets)
        case p2ps: P2PServiceBaseType => PointToPointObjectFactory.createP2Ps(p2ps)
      }

      // FIXME replace if already exists?
      HasXmlAny[A].setAny(a, Seq(element))

      a
    }

    def getPointToPointService(): Option[P2PServiceBaseType] = HasXmlAny[A].getAny(a).collectFirst {
      case JaxbElement(EVTS_QNAME, evts: EthernetVlanType)   => evts
      case JaxbElement(ETS_QNAME, ets: EthernetBaseType)     => ets
      case JaxbElement(P2PS_QNAME, p2ps: P2PServiceBaseType) => p2ps
    }
  }
}
