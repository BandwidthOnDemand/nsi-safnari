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
import org.ogf.schemas.nsi._2013._07.connection.types.ChildSummaryType
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultCriteriaType

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

  private val pointToPointObjectFactory = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory()

  implicit class HasAnyServiceOps[A: HasAnyService](a: A) {
    def withP2Ps(p2ps: P2PServiceBaseType): A = {
      // FIXME replace if already exists?
      // FIXME deal with subtypes of P2PServiceBaseType?
      implicitly[HasAnyService[A]].withAny(a, Seq(pointToPointObjectFactory.createP2Ps(p2ps)))
      a
    }

    def getP2Ps(): Option[P2PServiceBaseType] = {
      val qname = pointToPointObjectFactory.createP2Ps(null).getName()
      implicitly[HasAnyService[A]].getAny(a).collectFirst {
        case element: JAXBElement[_] if element.getName() == qname =>
          element.getValue().asInstanceOf[P2PServiceBaseType]
      }
    }
  }

  implicit val ReservationRequestCriteriaTypeHasAnyService = new HasAnyService[ReservationRequestCriteriaType] {
    def getAny(a: ReservationRequestCriteriaType) = a.getAny().asScala
    def withAny(a: ReservationRequestCriteriaType, any: Seq[AnyRef]) = a.withAny(any: _*)
  }
  implicit val ReservationConfirmCriteriaTypeHasAnyService = new HasAnyService[ReservationConfirmCriteriaType] {
    def getAny(a: ReservationConfirmCriteriaType) = a.getAny().asScala
    def withAny(a: ReservationConfirmCriteriaType, any: Seq[AnyRef]) = a.withAny(any: _*)
  }
  implicit val ChildSummaryTypeHasAnyService = new HasAnyService[ChildSummaryType] {
    def getAny(a: ChildSummaryType) = a.getAny().asScala
    def withAny(a: ChildSummaryType, any: Seq[AnyRef]) = a.withAny(any: _*)
  }
  implicit val QuerySummaryResultCriteriaTypeHasAnyService = new HasAnyService[QuerySummaryResultCriteriaType] {
    def getAny(a: QuerySummaryResultCriteriaType) = a.getAny().asScala
    def withAny(a: QuerySummaryResultCriteriaType, any: Seq[AnyRef]) = a.withAny(any: _*)
  }
}
