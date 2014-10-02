package nl.surfnet

import java.net.URI
import javax.xml.bind.JAXBElement
import javax.xml.namespace.QName
import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.nsiv2.messages.CorrelationId
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.TypeValuePairListType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import play.api.data.validation.ValidationError
import play.api.libs.json._
import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

package object safnari {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  private val UuidGenerator = Uuid.randomUuidGenerator

  def newConnectionId: ConnectionId = UuidGenerator().toString

  def valueFormat[T](message: String)(parse: String => Option[T], print: T => String): Format[T] = new Format[T] {
    override def reads(json: JsValue): JsResult[T] = json match {
      case JsString(s) => parse(s) match {
        case Some(t) => JsSuccess(t)
        case None    => JsError(Seq(JsPath() -> Seq(ValidationError(message, s))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
    override def writes(t: T): JsValue = JsString(print(t))
  }

  implicit val UriFormat: Format[URI] = valueFormat("error.expected.uri")(
    parse = s => Try(URI.create(s)).toOption,
    print = _.toASCIIString())
  implicit val CorrelationIdFormat: Format[CorrelationId] = valueFormat("error.expected.correlationId")(
    parse = CorrelationId.fromString,
    print = _.toString)

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

  implicit object XmlGregorianCalendarOrdering extends Ordering[XMLGregorianCalendar] {
    def compare(x: XMLGregorianCalendar, y: XMLGregorianCalendar): Int = x compare y
  }
  implicit object DateTimeOrdering extends Ordering[DateTime] {
    def compare(x: DateTime, y: DateTime): Int = x compareTo y
  }

  implicit class ReadableInstantOps(instant: org.joda.time.ReadableInstant) {
    def toSqlTimestamp = new java.sql.Timestamp(instant.getMillis())
  }

  private[this] val datatypeFactory = DatatypeFactory.newInstance()
  implicit class DateTimeOps(dt: org.joda.time.ReadableDateTime) {
    def toXmlGregorianCalendar = {
      val timezoneInMinutes = dt.getZone().getOffset(dt.getMillis()) / (60 * 1000)
      datatypeFactory.newXMLGregorianCalendar(
        dt.getYear(),
        dt.getMonthOfYear(),
        dt.getDayOfMonth(),
        dt.getHourOfDay(),
        dt.getMinuteOfHour(),
        dt.getSecondOfMinute(),
        dt.getMillisOfSecond(),
        timezoneInMinutes)
    }
  }

  implicit class XmlGregorianCalendarOps(dt: XMLGregorianCalendar) {
    def toDateTime = {
      val calendar = dt.toGregorianCalendar()
      new DateTime(calendar.getTimeInMillis(), DateTimeZone.forTimeZone(dt.toGregorianCalendar().getTimeZone()))
    }
  }
  implicit class ScheduleTypeOps(schedule: ScheduleType) {
    def startTime = Option(schedule.getStartTime).map(_.toDateTime)
    def endTime = Option(schedule.getEndTime).map(_.toDateTime)
  }
  implicit class OptionOps[A](value: Option[A]) {
    def toTry(ifNone: => Throwable): Try[A] = value.map(Success(_)).getOrElse(Failure(ifNone))
  }
  implicit class OptionTryOps[A](value: Option[Try[A]]) {
    def sequence: Try[Option[A]] = value match {
      case None    => Success(None)
      case Some(t) => t.map(Some(_))
    }
  }
  implicit val ReservationCriteriaConversion = Conversion.build[ReservationConfirmCriteriaType, ReservationRequestCriteriaType] { a =>
    Try(new ReservationRequestCriteriaType().
      withSchedule(a.getSchedule()).
      withAny(a.getAny()).
      withServiceType(a.getServiceType()).
      withVersion(a.getVersion()).
      tap(_.getOtherAttributes().putAll(a.getOtherAttributes())))
  } { b =>
    for {
      schedule <- Option(b.getSchedule()).toTry(ErrorMessageException("schedule is required"))
      serviceType <- Option(b.getServiceType()).toTry(ErrorMessageException("serviceType is required"))
    } yield {
      new ReservationConfirmCriteriaType().
        withSchedule(schedule).
        withAny(b.getAny()).
        withServiceType(b.getServiceType()).
        withVersion(if (b.getVersion() == null) 1 else b.getVersion()).
        tap(_.getOtherAttributes().putAll(b.getOtherAttributes()))
    }
  }

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
}
