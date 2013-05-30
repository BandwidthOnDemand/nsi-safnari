package nl.surfnet.safnari

import akka.actor._
import com.twitter.bijection.Injection
import org.ogf.schemas.nsi._2013._04.connection.types._
import java.net.URI
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

case class SegmentKnown(segmentId: ConnectionId)

class ConnectionEntity(id: ConnectionId, initialReserve: Reserve, newCorrelationId: () => CorrelationId, aggregatorNsa: String, nsiReplyToUri: URI, pceReplyUri: URI) {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), aggregatorNsa, provider.nsa, Some(nsiReplyToUri))
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), aggregatorNsa, requesterNSA, None)

  val rsm = new ReservationStateMachine(id, initialReserve, pceReplyUri, newCorrelationId, newNsiHeaders, { error =>
    new GenericFailedType().withConnectionId(id).withConnectionStates(connectionStates).
      withServiceException(new ServiceExceptionType().withErrorId(error.id).withText(error.text).withNsaId(aggregatorNsa))
  })

  private var otherStateMachines: Option[(ProvisionStateMachine, LifecycleStateMachine, DataPlaneStateMachine)] = None
  def psm = otherStateMachines.map(_._1)
  def lsm = otherStateMachines.map(_._2)
  def dsm = otherStateMachines.map(_._3)

  def process(message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val stateMachine: Option[FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = message match {
      // RSM messages
      case FromRequester(_: Reserve)               => Some(rsm)
      case FromRequester(_: ReserveCommit)         => Some(rsm)
      case FromRequester(_: ReserveAbort)          => Some(rsm)
      case FromProvider(_: ReserveConfirmed)       => Some(rsm)
      case FromProvider(_: ReserveFailed)          => Some(rsm)
      case FromProvider(_: ReserveCommitConfirmed) => Some(rsm)
      case FromProvider(_: ReserveCommitFailed)    => Some(rsm)
      case FromProvider(_: ReserveAbortConfirmed)  => Some(rsm)
      case FromProvider(_: ReserveTimeout)         => Some(???) // TODO
      case FromPce(_)                              => Some(rsm)

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

      case FromRequester(_: QuerySummary | _: QuerySummarySync | _: QueryRecursive) =>
        ???
    }

    stateMachine.flatMap(applyMessageToStateMachine(_, message))
  }

  private def applyMessageToStateMachine(stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage], message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val output = stateMachine.process(message) //.getOrElse(messageNotApplicable(message))

    output.foreach { messages =>
      messages.collectFirst {
        case ToRequester(confirmed: ReserveCommitConfirmed) => rsm.childConnections.map(kv => kv._1 -> kv._2.provider)
      }.foreach { children =>
        otherStateMachines = Some((
          new ProvisionStateMachine(id, newNsiHeaders, children),
          new LifecycleStateMachine(id, newNsiHeaders, children),
          new DataPlaneStateMachine(id, newNotifyHeaders, children)))
      }
    }

    output
  }

  def query = {
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
      withProvisionState(psm.map(_.provisionState).getOrElse(ProvisionStateEnumType.RELEASED)).
      withLifecycleState(lsm.map(_.lifecycleState).getOrElse(LifecycleStateEnumType.CREATED)).
      withDataPlaneStatus(dsm.map(_.dataPlaneStatus(version)).getOrElse(new DataPlaneStatusType()))
  }

  def segments: Seq[ConnectionData] = rsm.childConnectionStates.map {
    case (id, rs) => ConnectionData(id,
      lsm.map(_.childConnectionState(id)).getOrElse(LifecycleStateEnumType.CREATED),
      rs.jaxb,
      psm.map(_.childConnectionState(id)).getOrElse(ProvisionStateEnumType.RELEASED),
      dsm.map(_.childConnectionState(id)).getOrElse(false))
  }.toSeq

}
