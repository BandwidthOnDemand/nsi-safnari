package models

import org.ogf.schemas.nsi._2013._04.connection.types.ObjectFactory
import org.ogf.schemas.nsi._2013._04.framework.types.{ ObjectFactory => FTypesObjectFactory }
import org.w3c.dom.Document
import support._
import org.ogf.schemas.nsi._2013._04.connection.types._

sealed trait NsiRequesterOperation extends NsiMessage {
  override def headers: NsiHeaders = ???
  override def bodyDocument: Document = ???
}

object NsiRequesterOperation {
  import NsiMessage._

  case class ReserveConfirmed() extends NsiRequesterOperation
  case class ReserveFailed(override val headers: NsiHeaders, connectionId: String) extends NsiRequesterOperation {
    override def bodyDocument = {
      val factory = new ObjectFactory()

      val genericFailed = factory.createGenericFailedType()
        .withConnectionId(connectionId)
        .withConnectionStates(factory.createConnectionStatesType()
          .withDataPlaneStatus(factory.createDataPlaneStatusType()
            .withActive(false)
            .withVersion(0)
            .withVersionConsistent(true))
          .withLifecycleState(factory.createLifecycleStateType().withState(LifecycleStateEnumType.TERMINATED))
          .withProvisionState(factory.createProvisionStateType().withState(ProvisionStateEnumType.RELEASED))
          .withReservationState(factory.createReservationStateType().withState(ReservationStateEnumType.RESERVE_FAILED)))
        .withServiceException(new FTypesObjectFactory().createServiceExceptionType()
          .withErrorId("0600")
          .withNsaId("urn:ogf:surfnet.nl")
          .withText("Creating reservation is not supported yet"))

      marshal(factory.createReserveFailed(genericFailed))
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
