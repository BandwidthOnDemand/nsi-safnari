package nl.surfnet

import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.safnari.{Conversion, CorrelationId, Uuid}
import org.ogf.schemas.nsi._2013._04.connection.types.{ReservationConfirmCriteriaType, ReservationRequestCriteriaType}
import org.ogf.schemas.nsi._2013._04.framework.types.TypeValuePairListType
import scala.util.{Failure, Success, Try}
import org.ogf.schemas.nsi._2013._04.connection.types.ServiceAttributesType

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
      withBandwidth(a.getBandwidth()).
      withServiceAttributes(a.getServiceAttributes()).
      withPath(a.getPath()).
      withVersion(a.getVersion()))
  } { b =>
    (for {
      schedule <- Option(b.getSchedule())
      bandwidth <- Option(b.getBandwidth())
      serviceAttributes = Option(b.getServiceAttributes()).getOrElse(new ServiceAttributesType())
      path <- Option(b.getPath())
    } yield {
      new ReservationConfirmCriteriaType().
        withSchedule(schedule).
        withBandwidth(bandwidth).
        withServiceAttributes(serviceAttributes).
        withPath(path).
        withVersion(if (b.getVersion() == null) 0 else b.getVersion())
    }).toRight("cannot convert request criteria to confirm criteria. Missing fields?")
  }
}
