package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ReservationStateEnumType

case class ConnectionData(
    connectionId: ConnectionId,
    providerNsa: String,
    reservationState: ReservationStateEnumType,
    lifecycleState: LifecycleStateEnumType,
    provisionState: ProvisionStateEnumType,
    dataPlaneStatus: Boolean)
