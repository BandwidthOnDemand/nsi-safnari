package nl.surfnet.safnari

import akka.actor._
import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import play.api.Logger

class ConnectionEntity(val id: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve], newCorrelationId: () => CorrelationId, aggregatorNsa: String, nsiReplyToUri: URI, pceReplyUri: URI) {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), aggregatorNsa, provider.nsa, Some(nsiReplyToUri), NsiHeaders.ProviderProtocolVersion)
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), requesterNSA, aggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
  private var nextNotificationId: Int = 1
  private def newNotificationId() = {
    val r = nextNotificationId
    nextNotificationId += 1
    r
  }

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

  private var providerConversations: Map[CorrelationId, FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = Map.empty

  def process(message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val stateMachine: Option[FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = message match {
      case FromRequester(NsiProviderMessage(_, _: InitialReserve)) => Some(rsm)
      case FromRequester(NsiProviderMessage(_, _: ReserveCommit)) => Some(rsm)
      case FromRequester(NsiProviderMessage(_, _: ReserveAbort)) => Some(rsm)
      case FromRequester(NsiProviderMessage(_, _: Provision)) => psm
      case FromRequester(NsiProviderMessage(_, _: Release)) => psm
      case FromRequester(NsiProviderMessage(_, _: Terminate)) => lsm
      case FromRequester(NsiProviderMessage(_, _)) => None

      case FromPce(_) => Some(rsm)

      case FromProvider(NsiRequesterMessage(_, _: ReserveTimeout)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: DataPlaneStateChange)) => dsm
      case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) if error.error.getEvent() == EventEnumType.FORCED_END =>
        lsm
      case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: MessageDeliveryTimeout)) => Some(rsm)

      case FromProvider(NsiRequesterMessage(headers, _)) =>
        val stateMachine = providerConversations.get(headers.correlationId)
        if (stateMachine.isEmpty) Logger.debug(s"No active conversation for ${message.toShortString}")
        providerConversations -= headers.correlationId
        stateMachine

      case AckFromProvider(NsiProviderMessage(headers, _)) =>
        providerConversations.get(headers.correlationId)
    }

    stateMachine.flatMap(applyMessageToStateMachine(_, message))
  }

  private def applyMessageToStateMachine(stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage], message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val output = stateMachine.process(message)

    output.foreach { messages =>
      providerConversations ++= messages.collect {
        case message: ToProvider =>
          Logger.trace(s"Registering conversation for ${message.toShortString}")
          message.correlationId -> stateMachine
      }

      messages.collectFirst {
        case ToRequester(NsiRequesterMessage(_, confirmed: ReserveCommitConfirmed)) => rsm.childConnections.map(kv => kv._1 -> kv._2.provider)
      }.foreach { children =>
        otherStateMachines = Some((
          new ProvisionStateMachine(id, newNsiHeaders, children),
          new LifecycleStateMachine(id, newNsiHeaders, newNotifyHeaders, newNotificationId, children),
          new DataPlaneStateMachine(id, newNotifyHeaders, newNotificationId, children)))
      }
    }

    output
  }

  def query = {
    val children = rsm.childConnections.zipWithIndex.map {
      case ((id, segment), order) => new ChildSummaryType().
        withConnectionId(id).
        withProviderNSA(segment.provider.nsa).
        withPointToPointService(segment.serviceType.service).
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
