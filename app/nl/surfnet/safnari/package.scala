package nl.surfnet

import nl.surfnet.nsiv2.messages._

import java.net.URI
import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.ogf.schemas.nsi._2013._12.connection.types._
import play.api.data.validation.ValidationError
import play.api.libs.json._
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
    print = _.toASCIIString)
  implicit val CorrelationIdFormat: Format[CorrelationId] = valueFormat("error.expected.correlationId")(
    parse = CorrelationId.fromString,
    print = _.toString)

  implicit class AnyOps[A](a: A) {
    def tap(f: A => Unit): A = { f(a); a }
    def pp: A = { Console.err.println(a); a }
    def pp(prefix: String): A = { Console.err.println(s"$prefix: $a"); a }
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
    def toSqlTimestamp = new java.sql.Timestamp(instant.getMillis)
  }

  implicit class ScheduleTypeOps(schedule: ScheduleType) {
    def startTime = Option(schedule.getStartTime).map(_.toDateTime)
    def endTime = Option(schedule.getEndTime).map(_.toDateTime)
  }
  implicit class OptionOps[A](value: Option[A]) {
    def toTry(ifNone: => Throwable): Try[A] = value.map(Success(_)).getOrElse(Failure(ifNone))
  }
}
