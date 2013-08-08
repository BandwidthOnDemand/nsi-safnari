package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import javax.xml.bind.JAXBElement
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.connection.types.ChildSummaryType

trait HasAnyService[A] {
  def getAny(a: A): Seq[AnyRef]
  def withAny(a: A, any: Seq[AnyRef]): Unit
}
