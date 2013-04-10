package models

import org.ogf.schemas.nsi._2013._04.connection.types.ObjectFactory
import org.w3c.dom.Document

sealed trait NsiRequesterOperation extends NsiMessage {
  override def headers: NsiHeaders = ???
  override def bodyDocument: Document = ???
}

object NsiRequesterOperation {
  import NsiMessage._

  case class ReserveConfirmed() extends NsiRequesterOperation
  case class ReserveFailed(override val headers: NsiHeaders) extends NsiRequesterOperation {
    override def bodyDocument = {
      val factory = new ObjectFactory()
      val genericFailed = factory.createGenericFailedType();
      val response = factory.createReserveFailed(genericFailed)

      val doc = db.newDocument()
      marshaller.marshal(response, doc)
      doc
    }
  }
  case class ReserveCommitConfirmed() extends NsiRequesterOperation
  case class ReserveCommitFailed() extends NsiRequesterOperation
  case class ReserveAbortConfirmed() extends NsiRequesterOperation
  case class ReserveTimeout() extends NsiRequesterOperation

  case class ProvisionConfirmed() extends NsiRequesterOperation
  case class ReleaseConfirmed() extends NsiRequesterOperation
  case class TerminateConfirmed() extends NsiRequesterOperation

  case class QuerySummaryConfirmed() extends NsiRequesterOperation
  case class QuerySummaryFailed() extends NsiRequesterOperation
  case class QueryRecursiveConfirmed() extends NsiRequesterOperation
  case class QueryRecursiveFailed() extends NsiRequesterOperation

  case class ErrorEvent() extends NsiRequesterOperation
  case class DataPlaneStateChanged() extends NsiRequesterOperation
  case class MessageDeliveryTimeout() extends NsiRequesterOperation
}
