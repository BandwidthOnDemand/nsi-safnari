package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

case class ConnectionData(
    connectionId: Option[ConnectionId],
    providerNsa: String,
    reservationState: ReservationStateEnumType,
    lifecycleState: LifecycleStateEnumType,
    provisionState: ProvisionStateEnumType,
    dataPlaneStatus: Boolean,
    lastServiceException: Option[ServiceExceptionType])
