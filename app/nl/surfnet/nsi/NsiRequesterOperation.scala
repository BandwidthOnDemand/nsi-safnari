package nl.surfnet.nsi

import org.ogf.schemas.nsi._2013._04.connection.types.ObjectFactory
import org.ogf.schemas.nsi._2013._04.framework.types.{ ObjectFactory => FTypesObjectFactory }
import org.w3c.dom.Document
import org.ogf.schemas.nsi._2013._04.framework.types.{ ObjectFactory => FTypesObjectFactory }
import org.ogf.schemas.nsi._2013._04.connection.types._
import scala.collection.JavaConverters._

sealed trait NsiRequesterOperation extends NsiMessage {
  override def asDocument: Document = ???
  override def optionalConnectionId: Option[ConnectionId] = ???
}

object NsiRequesterOperation {
  import NsiMessage._

  case class ReserveConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation {
    override def asDocument = {
      val factory = new ObjectFactory()

      val confirmed = factory.createReserveConfirmedType()
        .withConnectionId(connectionId).withCriteria(factory.createReservationConfirmCriteriaType().withBandwidth(100)
          .withSchedule(factory.createScheduleType())
          .withServiceAttributes(new FTypesObjectFactory().createTypeValuePairListType())
          .withPath(factory.createPathType()
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSourceSTP(factory.createStpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("22"))
            .withDestSTP(factory.createStpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("33")))
          .withVersion(0))

      marshal(factory.createReserveConfirmed(confirmed))
    }
  }

  case class ReserveFailed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation {
    override def asDocument = {
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

  case class ReserveCommitConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation {
    override def asDocument = {
      val factory = new ObjectFactory()

      val confirmed = factory.createGenericConfirmedType().withConnectionId(connectionId)

      marshal(factory.createReserveCommitConfirmed(confirmed))
    }
  }

  case class ReserveCommitFailed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
  case class ReserveAbortConfirmed(correlationId: CorrelationId, connectionId: ConnectionId) extends NsiRequesterOperation
  case class ReserveTimeout(correlationId: CorrelationId) extends NsiRequesterOperation

  case class ProvisionConfirmed(correlationId: CorrelationId) extends NsiRequesterOperation
  case class ReleaseConfirmed(correlationId: CorrelationId) extends NsiRequesterOperation
  case class TerminateConfirmed(correlationId: CorrelationId) extends NsiRequesterOperation

  case class QuerySummaryConfirmed(correlationId: CorrelationId, reservations: Seq[QuerySummaryResultType]) extends NsiRequesterOperation {
    override def asDocument = {
      val factory = new ObjectFactory()
      val q = new QuerySummaryConfirmedType().withReservation(reservations.asJava)
      marshal(factory.createQuerySummaryConfirmed(q))
    }
  }
  case class QuerySummaryFailed(correlationId: CorrelationId) extends NsiRequesterOperation
  case class QueryRecursiveConfirmed(correlationId: CorrelationId) extends NsiRequesterOperation
  case class QueryRecursiveFailed(correlationId: CorrelationId) extends NsiRequesterOperation

  case class ErrorEvent(correlationId: CorrelationId) extends NsiRequesterOperation
  case class DataPlaneStateChanged(correlationId: CorrelationId) extends NsiRequesterOperation
  case class MessageDeliveryTimeout(correlationId: CorrelationId) extends NsiRequesterOperation
}
