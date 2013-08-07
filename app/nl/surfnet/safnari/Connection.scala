package nl.surfnet.safnari

import akka.actor._
import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

class ConnectionEntity(val id: ConnectionId, initialReserve: Reserve, newCorrelationId: () => CorrelationId, aggregatorNsa: String, nsiReplyToUri: URI, pceReplyUri: URI) {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), aggregatorNsa, provider.nsa, Some(nsiReplyToUri))
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), aggregatorNsa, requesterNSA, None)

  private val criteria: ReservationConfirmCriteriaType = Conversion.invert(initialReserve.body.getCriteria()).fold(error => throw new IllegalArgumentException(s"Bad initial reservation criteria: $error"), identity)
  private val p2ps: P2PServiceBaseType = criteria.getP2Ps().getOrElse(throw new IllegalArgumentException(s"initial criteria does not contain P2P service: $criteria"))

  val rsm = new ReservationStateMachine(id, initialReserve, criteria, pceReplyUri, newCorrelationId, newNsiHeaders, { error =>
    new GenericFailedType().
      withConnectionId(id).
      withConnectionStates(connectionStates).
      withServiceException(new ServiceExceptionType().
        withErrorId(error.id).
        withText(error.text).
        withNsaId(aggregatorNsa))
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
      case FromProvider(_: ReserveTimeout)         => Some(rsm)
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
    val output = stateMachine.process(message)

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
      case ((id, segment), order) => new ChildSummaryType().
        withConnectionId(id).
        withProviderNSA(segment.provider.nsa).
        withP2Ps(new P2PServiceBaseType().
            withCapacity(p2ps.getCapacity()).
            withDirectionality(p2ps.getDirectionality()).
            withSymmetricPath(p2ps.isSymmetricPath()).
            withEro(p2ps.getEro()).
            withSourceSTP(segment.sourceStp).
            withDestSTP(segment.destinationStp)).
        withOrder(order)
    }

    val criteria = rsm.criteria
    new QuerySummaryResultType().
      withGlobalReservationId(initialReserve.body.getGlobalReservationId()).
      withDescription(initialReserve.body.getDescription()).
      withConnectionId(id).
      withCriteria(new QuerySummaryResultCriteriaType().
        withSchedule(criteria.getSchedule()).
        withChildren(new ChildSummaryListType().withChild(children.toSeq: _*))).
      withRequesterNSA(requesterNSA).
      withConnectionStates(connectionStates)
  }

  def connectionStates: ConnectionStatesType = {
    val version = rsm.version
    new ConnectionStatesType().
      withReservationState(rsm.reservationState).
      withProvisionState(psm.map(_.provisionState).getOrElse(ProvisionStateEnumType.RELEASED)).
      withLifecycleState(lsm.map(_.lifecycleState).getOrElse(LifecycleStateEnumType.CREATED)).
      withDataPlaneStatus(dsm.map(_.dataPlaneStatus(version)).getOrElse(new DataPlaneStatusType()))
  }

  def segments: Seq[ConnectionData] = rsm.childConnections.map {
    case (id, segment) => ConnectionData(
      id,
      segment.provider.nsa,
      rsm.childConnectionState(id),
      lsm.map(_.childConnectionState(id)).getOrElse(LifecycleStateEnumType.CREATED),
      psm.map(_.childConnectionState(id)).getOrElse(ProvisionStateEnumType.RELEASED),
      dsm.map(_.childConnectionState(id)).getOrElse(false))
  }.toSeq

}
