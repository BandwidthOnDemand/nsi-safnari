package presenters

import org.ogf.schemas.nsi._2013._12.connection.types.LifecycleStateEnumType._
import org.ogf.schemas.nsi._2013._12.connection.types.{ConnectionStatesType, LifecycleStateEnumType, ProvisionStateEnumType, ReservationStateEnumType}

case class Nsi2StatusPresenter(lifecycle: LifecycleStateEnumType,
                               reservation: ReservationStateEnumType,
                               provision: ProvisionStateEnumType,
                               dataPlaneActive: Boolean) {

  def status = notifiableStates mkString ", "

  private def notifiableStates = lifecycle match {
    case TERMINATED => List(lifecycle.value)
    case CREATED => reservationState ++ provisionState
    case _ => reservationState :+ lifecycle.value
  }

  private def reservationState = if (reservation != ReservationStateEnumType.RESERVE_START) List(reservation.value) else Nil
  private def provisionState = if (provision != null) List(provision.value, dataPlaneState) else Nil
  private def dataPlaneState = if (dataPlaneActive) "Active" else "Inactive"
}

object Nsi2StatusPresenter {
  def apply(states: ConnectionStatesType) = new Nsi2StatusPresenter(states.getLifecycleState,
                                                                    states.getReservationState,
                                                                    states.getProvisionState,
                                                                    states.getDataPlaneStatus.isActive)
}
