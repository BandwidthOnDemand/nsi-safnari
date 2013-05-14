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
    psm ask data.providers
    lsm ask data.providers
    dsm ask data.providers
  })

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

    // LSM messages
    case message @ FromRequester(_: Terminate) => lsm ask message foreach (sender ! _)
    case message @ FromProvider(_: TerminateConfirmed) => lsm ask message foreach (sender ! _)

    // Data Plane Status messages
    case message @ FromProvider(_: DataPlaneStateChanged) => dsm ask message foreach (sender ! _)

    case 'query => sender ! query

    case message => message.pp("unexpected")
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
      withReservationState(new ReservationStateType().withVersion(version).withState(rsm.stateName.jaxb)).
      withProvisionState(psm.provisionState(version)).
      withLifecycleState(lsm.lifecycleState(version)).
      withDataPlaneStatus(dsm.dataPlaneStatus(version))
  }
}
