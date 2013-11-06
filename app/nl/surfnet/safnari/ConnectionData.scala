package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationStateEnumType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

case class ConnectionData(
    connectionId: Option[ConnectionId],
    providerNsa: String,
    reservationState: ReservationStateEnumType,
    lifecycleState: LifecycleStateEnumType,
    provisionState: ProvisionStateEnumType,
    dataPlaneStatus: Boolean,
    lastServiceException: Option[ServiceExceptionType])
