package nl.surfnet.nsi

import org.ogf.schemas.nsi._2013._04.connection.types.ObjectFactory
import org.ogf.schemas.nsi._2013._04.framework.types.{ ObjectFactory => FTypesObjectFactory }
import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.framework.types.{ObjectFactory => FTypesObjectFactory}
import org.ogf.schemas.nsi._2013._04.connection.types._

sealed trait NsiRequesterOperation extends NsiMessage {
  override def headers: NsiHeaders = ???
  override def bodyDocument: Document = ???
  override def optionalConnectionId: Option[ConnectionId] = ???
}

object NsiRequesterOperation {
  import NsiMessage._

  case class ReserveConfirmed(override val headers: NsiHeaders) extends NsiRequesterOperation

  case class ReserveFailed(override val headers: NsiHeaders, connectionId: String) extends NsiRequesterOperation with Response {
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

  case class QuerySummaryConfirmed(override val headers: NsiHeaders, connectionIds: Seq[(String, ReservationState)]) extends NsiRequesterOperation with Response {
    override def bodyDocument = {
      val factory = new ObjectFactory()
      val q = factory.createQuerySummaryConfirmedType()
      connectionIds.map { case (id, reservationState) =>
        q.withReservation(factory.createQuerySummaryResultType()
          .withConnectionId(id)
          .withRequesterNSA("urn:ogf:network:nsa:surfnet-nsi-requester")
          .withConnectionStates(factory.createConnectionStatesType()
            .withDataPlaneStatus(factory.createDataPlaneStatusType()
              .withActive(false)
              .withVersion(0)
              .withVersionConsistent(true))
            .withLifecycleState(factory.createLifecycleStateType().withState(LifecycleStateEnumType.TERMINATED))
            .withProvisionState(factory.createProvisionStateType().withState(ProvisionStateEnumType.RELEASED))
            .withReservationState(factory.createReservationStateType().withState(reservationState.jaxb)))
          .withCriteria(factory.createReservationConfirmCriteriaType()
            .withBandwidth(100)
            .withSchedule(factory.createScheduleType())
            .withServiceAttributes(new FTypesObjectFactory().createTypeValuePairListType())
            .withPath(factory.createPathType()
              .withDirectionality(DirectionalityType.BIDIRECTIONAL)
              .withSourceSTP(factory.createStpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("22"))
              .withDestSTP(factory.createStpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("33")))
            .withVersion(0)))
      }
      marshal(factory.createQuerySummaryConfirmed(q))
    }
  }
  case class QuerySummaryFailed() extends NsiRequesterOperation
  case class QueryRecursiveConfirmed() extends NsiRequesterOperation
  case class QueryRecursiveFailed() extends NsiRequesterOperation

  case class ErrorEvent() extends NsiRequesterOperation
  case class DataPlaneStateChanged() extends NsiRequesterOperation
  case class MessageDeliveryTimeout() extends NsiRequesterOperation
}
