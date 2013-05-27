package nl.surfnet.safnari

import akka.actor._
import com.twitter.bijection.Injection
import org.ogf.schemas.nsi._2013._04.connection.types._
import java.net.URI
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

sealed trait Message
case class FromRequester(message: NsiProviderOperation) extends Message
case class ToRequester(message: NsiRequesterOperation) extends Message
case class FromProvider(message: NsiRequesterOperation) extends Message
case class ToProvider(message: NsiProviderOperation, provider: ProviderEndPoint) extends Message
case class FromPce(message: PceResponse) extends Message
case class ToPce(message: PceRequest) extends Message

case class SegmentKnown(segmentId: ConnectionId)

class ConnectionActor(id: ConnectionId, initialReserve: Reserve, newCorrelationId: () => CorrelationId, outbound: (Message, ActorRef) => Unit, nsiReplyToUri: URI, pceReplyUri: URI) extends Actor {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), "NSA-ID", provider.nsa, Some(nsiReplyToUri))
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), "NSA-ID", requesterNSA, None)
  val psm = new ProvisionStateMachine(id, newNsiHeaders, message => outbound(message, self))
  val lsm = new LifecycleStateMachine(id, newNsiHeaders, message => outbound(message, self))
  val dsm = new DataPlaneStateMachine(id, newNotifyHeaders, message => outbound(message, self))
  val rsm = new ReservationStateMachine(id, initialReserve, pceReplyUri, newCorrelationId, newNsiHeaders, message => outbound(message, self), data => {
    psm process data.children
    lsm process data.children
    dsm process data.children
  }, error => {
    new GenericFailedType().withConnectionId(id).withConnectionStates(connectionStates).
      withServiceException(new ServiceExceptionType().withErrorId(error.id).withText(error.text).withNsaId("NSA-ID"))
  })

  override def receive = {
    case 'query                     => sender ! query
    case 'querySegments             => sender ! segments

    case SegmentKnown(connectionId) => sender ! rsm.segmentKnown(connectionId)

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
        case FromProvider(_: DataPlaneStateChange)   => dsm

        // PSM messages
        case FromRequester(_: Provision)             => psm
        case FromRequester(_: Release)               => psm
        case FromProvider(_: ProvisionConfirmed)     => psm
        case FromProvider(_: ReleaseConfirmed)       => psm

        // LSM messages
        case FromRequester(_: Terminate)             => lsm
        case FromProvider(_: TerminateConfirmed)     => lsm
      }

      val replies = stateMachine.process(message).getOrElse(messageNotApplicable(message))
      replies foreach (sender ! _)
  }

  private def query = {
    val children = rsm.childConnections.zipWithIndex.map {
      case ((id, segment), order) => new SummaryPathType().
        withConnectionId(id).
        withProviderNSA(segment.provider.nsa).
        withPath(new PathType().
            withSourceSTP(segment.sourceStp).
            withDestSTP(segment.destinationStp).
            withDirectionality(DirectionalityType.BIDIRECTIONAL)).
        withOrder(order)
    }

    new QuerySummaryResultType().
      withGlobalReservationId(initialReserve.body.getGlobalReservationId()).
      withDescription(initialReserve.body.getDescription()).
      withConnectionId(id).
      withCriteria(rsm.criteria).
      withRequesterNSA(requesterNSA).
      withConnectionStates(connectionStates).
      withChildren(new ChildSummaryListType().withChild(children.toSeq: _*))
  }

  def connectionStates: ConnectionStatesType = {
    val version = rsm.version
    new ConnectionStatesType().
      withReservationState(rsm.reservationState).
      withProvisionState(psm.provisionState).
      withLifecycleState(lsm.lifecycleState).
      withDataPlaneStatus(dsm.dataPlaneStatus(version))
  }

  private def segments: Seq[ConnectionData] = rsm.childConnectionStates.map {
    case (id, rs) => ConnectionData(id, lsm.childConnectionState(id), rs.jaxb, psm.childConnectionState(id), dsm.childConnectionState(id))
  }.toSeq

  private def messageNotApplicable(message: Any) = Vector(message match {
    case FromRequester(message) => ServiceException(message.headers.asReply, NsiError.InvalidState.toServiceException("NSA-ID"))
    case FromProvider(message)  => ServiceException(message.headers.asReply, NsiError.InvalidState.toServiceException("NSA-ID"))
    case FromPce(message)       => 400
  })
}
