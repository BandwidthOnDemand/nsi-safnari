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

class ConnectionActor(id: ConnectionId, requesterNSA: String, initialReserve: Reserve, newCorrelationId: () => CorrelationId, outbound: ActorRef, pceReplyUri: URI) extends Actor {
  val psm = new ProvisionStateMachine(id, newCorrelationId, outbound ! _)
  val lsm = new LifecycleStateMachine(id, newCorrelationId, outbound ! _)
  val dsm = new DataPlaneStateMachine(id, newCorrelationId, outbound ! _)
  val rsm = new ReservationStateMachine(id, initialReserve, newCorrelationId, outbound ! _, pceReplyUri, data => {
    psm process data.providers
    lsm process data.providers
    dsm process data.providers
  })

  override def receive = {
    case 'query => sender ! query

    case message =>
      val stateMachine: FiniteStateMachine[_, _] = message match {
        // RSM messages
        case FromRequester(_: Reserve)               => rsm
        case FromRequester(_: ReserveCommit)         => rsm
        case FromRequester(_: ReserveAbort)          => rsm
        case FromProvider(_: ReserveConfirmed)       => rsm
        case FromProvider(_: ReserveFailed)          => rsm
        case FromProvider(_: ReserveCommitConfirmed) => rsm
        case FromProvider(_: ReserveCommitFailed)    => rsm
        case FromProvider(_: ReserveAbortConfirmed)  => rsm
        case FromProvider(_: ReserveTimeout)         => ??? // TODO
        case FromPce(_)                              => rsm
        case ToPce(_)                                => rsm

        // Data Plane Status messages
        case FromProvider(_: DataPlaneStateChanged)  => dsm

        // PSM messages
        case FromRequester(_: Provision)             => psm
        case FromRequester(_: Release)               => psm
        case FromProvider(_: ProvisionConfirmed)     => psm
        case FromProvider(_: ReleaseConfirmed)       => psm

        // LSM messages
        case FromRequester(_: Terminate)             => lsm
        case FromProvider(_: TerminateConfirmed)     => lsm
      }
      stateMachine process message foreach (sender ! _)
  }

  private def query = {
    new QuerySummaryResultType().
      withGlobalReservationId(initialReserve.body.getGlobalReservationId()).
      withDescription(initialReserve.body.getDescription()).
      withConnectionId(id).
      withCriteria(rsm.criteria).
      withRequesterNSA(requesterNSA).
      withConnectionStates(connectionStates).
      withChildren(null /*TODO*/ )
  }

  def connectionStates = {
    val version = rsm.version
    new ConnectionStatesType().
      withReservationState(rsm.reservationState).
      withProvisionState(psm.provisionState(version)).
      withLifecycleState(lsm.lifecycleState(version)).
      withDataPlaneStatus(dsm.dataPlaneStatus(version))
  }
}
