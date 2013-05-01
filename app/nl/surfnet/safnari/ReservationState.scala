package nl.surfnet.safnari

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
case object CheckingReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVE_CHECKING
}
case object HeldReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVE_HELD
}
case object CommittingReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVE_COMMITTING
}
case object AbortingReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVE_ABORTING
}
case object TimeoutReservationState extends ReservationState {
  def jaxb = ReservationStateEnumType.RESERVE_TIMEOUT
}