package nl.surfnet.safnari

import javax.xml.bind.JAXBElement
import org.ogf.schemas.nsi._2013._07.connection.types.ChildSummaryType
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultCriteriaType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationRequestCriteriaType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import scala.collection.JavaConverters._

/**
 * Type class for JAXB generated types that have an XML any element.
 */
trait HasXmlAny[A] {
  def getAny(a: A): Seq[AnyRef]
  def setAny(a: A, any: Seq[AnyRef]): Unit
}
object HasXmlAny {
  def apply[A](implicit hasXmlAny: HasXmlAny[A]) = hasXmlAny

  private def build[A](get: A => Seq[AnyRef], set: (A, Seq[AnyRef]) => Unit): HasXmlAny[A] = new HasXmlAny[A] {
    def getAny(a: A) = get(a)
    def setAny(a: A, any: Seq[AnyRef]) = set(a, any)
  }

  implicit val ChildSummaryType: HasXmlAny[ChildSummaryType] = build(_.getAny.asScala, (a, any) => a.getAny.addAll(any.asJava))
  implicit val QuerySummaryResultCriteriaType: HasXmlAny[QuerySummaryResultCriteriaType] = build(_.getAny.asScala, (a, any) => a.getAny.addAll(any.asJava))
  implicit val ReservationConfirmCriteriaType: HasXmlAny[ReservationConfirmCriteriaType] = build(_.getAny.asScala, (a, any) => a.getAny.addAll(any.asJava))
  implicit val ReservationRequestCriteriaType: HasXmlAny[ReservationRequestCriteriaType] = build(_.getAny.asScala, (a, any) => a.getAny.addAll(any.asJava))
}
