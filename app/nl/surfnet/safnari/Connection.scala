package nl.surfnet.safnari

import akka.actor._
import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

class ConnectionEntity(val id: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve], newCorrelationId: () => CorrelationId, aggregatorNsa: String, nsiReplyToUri: URI, pceReplyUri: URI) {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), aggregatorNsa, provider.nsa, Some(nsiReplyToUri), NsiHeaders.ProviderProtocolVersion)
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), requesterNSA, aggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)

  val rsm = new ReservationStateMachine(id, initialReserve, pceReplyUri, newCorrelationId, newNsiHeaders, { error =>
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
      case FromRequester(NsiProviderMessage(_, _: InitialReserve)) => Some(rsm)
      case FromRequester(NsiProviderMessage(_, _: ReserveCommit)) => Some(rsm)
      case FromRequester(NsiProviderMessage(_, _: ReserveAbort)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: ReserveConfirmed)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: ReserveFailed)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: ReserveCommitConfirmed)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: ReserveCommitFailed)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: ReserveAbortConfirmed)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: ReserveTimeout)) => Some(rsm)
      case FromPce(_) => Some(rsm)

      // Data Plane Status messages
      case FromProvider(NsiRequesterMessage(_, _: DataPlaneStateChange)) => dsm

      // PSM messages
      case FromRequester(NsiProviderMessage(_, _: Provision)) => psm
      case FromRequester(NsiProviderMessage(_, _: Release)) => psm
      case FromProvider(NsiRequesterMessage(_, _: ProvisionConfirmed)) => psm
      case FromProvider(NsiRequesterMessage(_, _: ReleaseConfirmed)) => psm

      // LSM messages
      case FromRequester(NsiProviderMessage(_, _: Terminate)) => lsm
      case FromProvider(NsiRequesterMessage(_, _: TerminateConfirmed)) => lsm
    }

    stateMachine.flatMap(applyMessageToStateMachine(_, message))
  }

  private def applyMessageToStateMachine(stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage], message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val output = stateMachine.process(message)

    output.foreach { messages =>
      messages.collectFirst {
        case ToRequester(NsiRequesterMessage(_, confirmed: ReserveCommitConfirmed)) => rsm.childConnections.map(kv => kv._1 -> kv._2.provider)
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
        withPointToPointService(segment.service).
        withOrder(order)
    }

    val criteria = rsm.criteria
    new QuerySummaryResultType().
      withGlobalReservationId(initialReserve.body.body.getGlobalReservationId()).
      withDescription(initialReserve.body.body.getDescription()).
      withConnectionId(id).
      withCriteria(new QuerySummaryResultCriteriaType().
        withSchedule(criteria.getSchedule()).
        withServiceType(criteria.getServiceType()).
        withPointToPointService(initialReserve.body.service).
        withChildren(new ChildSummaryListType().withChild(children.toSeq: _*)).
        tap(_.getOtherAttributes().putAll(criteria.getOtherAttributes()))).
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
