package nl.surfnet.nsi

import org.ogf.schemas.nsi._2013._04.connection.types.ReservationStateEnumType

sealed trait ReservationState {
  def jaxb: ReservationStateEnumType
}
case object InitialReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.INITIAL
}
case object FailedReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVE_FAILED
}
case object ReservedReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVED
}
