package nl.surfnet

import nl.surfnet.nsiv2.messages._

import java.net.URI
import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.ogf.schemas.nsi._2013._12.connection.types._
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

  implicit class TryOps[A](a: Try[A]) {
    def toEither: Either[Throwable, A] = a match {
      case Failure(t) => Left(t)
      case Success(a) => Right(a)
    }
  }

  implicit class ScheduleTypeOps(schedule: ScheduleType) {
    def startTime = Option(schedule.getStartTime).map(_.toDateTime)
    def endTime = Option(schedule.getEndTime).map(_.toDateTime)
  }
  implicit class OptionOps[A](value: Option[A]) {
    def toTry(ifNone: => Throwable): Try[A] = value.map(Success(_)).getOrElse(Failure(ifNone))
  }
}
