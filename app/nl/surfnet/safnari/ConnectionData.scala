package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ReservationStateEnumType

case class ConnectionData(
    connectionId: ConnectionId,
    lifecycleState: LifecycleStateEnumType,
    reservationState: ReservationStateEnumType,
    provisionState: ProvisionStateEnumType,
    dataPlaneStatus: Boolean)