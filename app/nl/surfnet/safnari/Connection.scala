package nl.surfnet.safnari

import akka.actor._
import com.twitter.bijection.Injection
import org.ogf.schemas.nsi._2013._04.connection.types._
import java.net.URI
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

case class FromRequester(message: NsiProviderOperation)
case class ToRequester(message: NsiRequesterOperation)
case class FromProvider(message: NsiRequesterOperation)
case class ToProvider(message: NsiProviderOperation, provider: ProviderEndPoint)
case class FromPce(message: PceResponse)
case class ToPce(message: PathComputationRequest)

class ConnectionActor(id: ConnectionId, requesterNSA: String, newCorrelationId: () => CorrelationId, outbound: ActorRef, pceReplyUri: URI) extends Actor {
  val psm = new ProvisionStateMachine(newCorrelationId, outbound ! _)
  val rsm = new ReservationStateMachine(id, requesterNSA, newCorrelationId, outbound ! _, pceReplyUri, data => psm ask data.providers)

  override def receive = {
    // RSM messages
    case message @ FromRequester(_: Reserve) => rsm ask message foreach (sender ! _)
    case message @ FromRequester(_: ReserveCommit) => rsm ask message foreach (sender ! _)
    case message @ FromRequester(_: ReserveAbort) => rsm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReserveConfirmed) => rsm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReserveFailed) => rsm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReserveCommitConfirmed) => rsm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReserveCommitFailed) => rsm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReserveAbortConfirmed) => rsm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReserveTimeout) => ??? // TODO
    case message: FromPce => rsm ask message foreach (sender ! _)
    case message: ToPce => rsm ask message foreach (sender ! _)

    // PSM messages
    case message @ FromRequester(_: Provision) => psm ask message foreach (sender ! _)
    case message @ FromRequester(_: Release) => psm ask message foreach (sender ! _)
    case message @ FromProvider(_: ProvisionConfirmed) => psm ask message foreach (sender ! _)
    case message @ FromProvider(_: ReleaseConfirmed) => psm ask message foreach (sender ! _)

    case 'query => sender ! query

    case message => message.pp("unexpected")
    //    case message => rsm ask message foreach (sender ! _)
  }

  private def query = {
    new QuerySummaryResultType().
        withGlobalReservationId(rsm.globalReservationId.orNull).
        withDescription(rsm.description.orNull).
        withConnectionId(id).
        withCriteria(rsm.criteria).
        withRequesterNSA(requesterNSA).
        withConnectionStates(connectionStates).
        withChildren(null /*TODO*/ )
  }

  def connectionStates = {
    val version = rsm.version
    new ConnectionStatesType().
      withReservationState(new ReservationStateType().withVersion(version).withState(rsm.stateName.jaxb)).
      withProvisionState(psm.provisionState(version)).
      withLifecycleState(new LifecycleStateType().withVersion(version).withState(LifecycleStateEnumType.INITIAL /*TODO*/ )).
      withDataPlaneStatus(new DataPlaneStatusType().withVersion(version).withActive(false))
  }

}


sealed trait Connection {
  def id: ConnectionId
}
case class NewConnection(id: ConnectionId) extends Connection
case class ExistingConnection(
  id: ConnectionId,
  reserveCorrelationId: CorrelationId,
  globalReservationId: Option[String],
  description: Option[String],
  criteria: ReservationConfirmCriteriaType,
  segments: Map[CorrelationId, ComputedSegment] = Map.empty,
  connections: Map[ConnectionId, CorrelationId] = Map.empty,
  downstreamConnections: Map[ConnectionId, ReservationState] = Map.empty) extends Connection {

  def awaitingConnectionId = segments.keySet -- connections.values

  def aggregatedReservationState: ReservationState =
    if (awaitingConnectionId.isEmpty && downstreamConnections.isEmpty) CheckingReservationState
    else if (awaitingConnectionId.nonEmpty || downstreamConnections.values.exists(_ == CheckingReservationState)) CheckingReservationState
    else if (downstreamConnections.values.exists(_ == FailedReservationState)) FailedReservationState
    else if (downstreamConnections.values.forall(_ == HeldReservationState)) HeldReservationState
    else if (downstreamConnections.values.exists(_ == CommittingReservationState)) CommittingReservationState
    else if (downstreamConnections.values.exists(_ == AbortingReservationState)) CommittingReservationState /* FIXME really? */
    else if (downstreamConnections.values.forall(_ == ReservedReservationState)) ReservedReservationState
    else ???

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId, reservationState: ReservationState): ExistingConnection = {
    require(awaitingConnectionId.contains(correlationId), s"bad correlationId: $correlationId, awaiting $awaitingConnectionId")
    require(!downstreamConnections.contains(connectionId), s"duplicate connectionId: $connectionId, already have $downstreamConnections")
    copy(
      connections = connections + (connectionId -> correlationId),
      downstreamConnections = downstreamConnections + (connectionId -> reservationState))
  }

  def providers = connections.map {
    case (connectionId, correlationId) =>
      connectionId -> segments(correlationId).provider
  }
}
