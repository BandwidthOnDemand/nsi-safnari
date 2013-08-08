package nl.surfnet.safnari

import javax.xml.bind.JAXBElement
import org.ogf.schemas.nsi._2013._07.connection.types.ChildSummaryType
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultCriteriaType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationRequestCriteriaType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import scala.collection.JavaConverters._

trait HasAnyService[A] {
  def getAny(a: A): Seq[AnyRef]
  def withAny(a: A, any: Seq[AnyRef]): Unit
}
object HasAnyService {
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
