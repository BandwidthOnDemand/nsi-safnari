package nl.surfnet.safnari

import akka.actor._
import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import play.api.Logger
import scala.util.Try

class ConnectionEntity(val id: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve], newCorrelationId: () => CorrelationId, aggregatorNsa: String, nsiReplyToUri: URI, pceReplyUri: URI) {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), aggregatorNsa, provider.nsa, Some(nsiReplyToUri), NsiHeaders.ProviderProtocolVersion)
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), requesterNSA, aggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
  private var nextNotificationId: Int = 1
  private var nsiNotifications: List[NsiNotification] = Nil
  private def newNotificationId() = {
    val r = nextNotificationId
    nextNotificationId += 1
    r
  }
  private var mostRecentChildExceptions = Map.empty[ConnectionId, ServiceExceptionType]

  val rsm = new ReservationStateMachine(id, initialReserve, pceReplyUri, newCorrelationId, newNsiHeaders, newNotificationId, { error =>
    new GenericFailedType().
      withConnectionId(id).
      withConnectionStates(connectionStates).
      withServiceException(new ServiceExceptionType().
        withErrorId(error.id).
        withText(error.text).
        withNsaId(aggregatorNsa))
  })

  private val globalReservationId: Option[GlobalReservationId] = Try(URI.create(initialReserve.body.body.getGlobalReservationId())).toOption

  private var otherStateMachines: Option[(ProvisionStateMachine, LifecycleStateMachine, DataPlaneStateMachine)] = None
  def psm = otherStateMachines.map(_._1)
  def lsm = otherStateMachines.map(_._2)
  def dsm = otherStateMachines.map(_._3)

  private var providerConversations: Map[CorrelationId, FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = Map.empty

  def queryRecursive(message: FromRequester): Option[Seq[OutboundMessage]] = message match {
    case FromRequester(pm @ NsiProviderMessage(_, QueryRecursive(ids))) =>
      require(ids.fold(true) {
        case Left(connectionIds) => connectionIds.contains(id)
        case Right(globalReservationIds) => globalReservationIds.contains(globalReservationId)
      })

      // FIXME - what if we got a query recursive when child connection ids are not known yet ?
      val qrsm = new QueryRecursiveStateMachine(
        id,
        pm.asInstanceOf[NsiProviderMessage[QueryRecursive]],
        initialReserve,
        connectionStates,
        rsm.childConnections.map(kv => kv._2.getOrElse(sys.error("Now what??")) -> kv._1.provider).toMap,
        newNsiHeaders)

      val output = qrsm.process(message)

      output foreach (registerProviderConversations(_, qrsm))

      output
  }

  def queryRecursiveResult(message: FromProvider): Option[Seq[OutboundMessage]] = message match {
    case FromProvider(NsiRequesterMessage(_, _: QueryRecursiveConfirmed)) | FromProvider(NsiRequesterMessage(_, _: QueryRecursiveFailed)) =>
      val qrsm = providerConversations.get(message.correlationId)
      providerConversations -= message.correlationId

      qrsm.flatMap(_.process(message))
  }

  def process(message: InboundMessage): Option[Seq[OutboundMessage]] = {
    message match {
      case AckFromProvider(NsiProviderMessage(_, ServiceException(exception))) =>
        Option(exception.getConnectionId()).foreach { connectionId =>
          mostRecentChildExceptions += connectionId -> exception;
        }
      case failed: MessageDeliveryFailure =>
        failed.connectionId.foreach { connectionId =>
          mostRecentChildExceptions += connectionId -> NsiError.ChildError.toServiceException("").withText(failed.toShortString)
        }
      case _ =>
    }

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
      case FromProvider(NsiRequesterMessage(_, _: ErrorEvent)) => None
      case FromProvider(NsiRequesterMessage(_, _: MessageDeliveryTimeout)) => None

      case FromProvider(NsiRequesterMessage(headers, _)) =>
        val stateMachine = providerConversations.get(headers.correlationId)
        if (stateMachine.isEmpty) Logger.debug(s"No active conversation for reply ${message.toShortString}")
        providerConversations -= headers.correlationId
        stateMachine

      case AckFromProvider(NsiProviderMessage(headers, _)) =>
        val stateMachine = providerConversations.get(headers.correlationId)
        if (stateMachine.isEmpty) Logger.debug(s"No active conversation for ack ${message.toShortString}")
        stateMachine

      case _: MessageDeliveryFailure =>
        None
    }

    stateMachine.flatMap(applyMessageToStateMachine(_, message)).orElse(handleUnhandledProviderNotifications(message))
  }

  def process(message: OutboundMessage): Unit = message match {
    case ToRequester(NsiRequesterMessage(headers, notification: NsiNotification)) => nsiNotifications = notification :: nsiNotifications
    case _ =>
  }

  private def applyMessageToStateMachine(stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage], message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val output = stateMachine.process(message)

    output.foreach { messages =>
      registerProviderConversations(messages, stateMachine)

      messages.collectFirst {
        case ToRequester(NsiRequesterMessage(_, confirmed: ReserveCommitConfirmed)) =>
          rsm.childConnections.map(kv => kv._2.getOrElse(throw new IllegalStateException("reserveConfirmed with unknown child connectionId")) -> kv._1.provider).toMap
      }.foreach { children =>
        otherStateMachines = Some((
          new ProvisionStateMachine(id, newNsiHeaders, children),
          new LifecycleStateMachine(id, newNsiHeaders, newNotifyHeaders, newNotificationId, children),
          new DataPlaneStateMachine(id, newNotifyHeaders, newNotificationId, children)))
      }
    }

    output
  }

  private def registerProviderConversations(messages: Seq[OutboundMessage], stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage]): Unit = {
      providerConversations ++= messages.collect {
        case message: ToProvider =>
          Logger.trace(s"Registering conversation for ${message.toShortString}")
          message.correlationId -> stateMachine
      }
  }

  private def handleUnhandledProviderNotifications(message: InboundMessage): Option[Seq[OutboundMessage]] = message match {
    case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) =>
      val event = ErrorEvent(new ErrorEventType()
        .withConnectionId(id)
        .withNotificationId(newNotificationId())
        .withTimeStamp(error.error.getTimeStamp())
        .withEvent(error.error.getEvent())
        .withAdditionalInfo(error.error.getAdditionalInfo()))
      if (error.error.getServiceException() ne null) {
        event.error.withServiceException(new ServiceExceptionType()
          .withConnectionId(id)
          .withNsaId(aggregatorNsa)
          .withErrorId(error.error.getServiceException().getErrorId())
          .withText(error.error.getServiceException().getText())
          .withServiceType(error.error.getServiceException().getServiceType()) // FIXME own service type?
          .withChildException(error.error.getServiceException()))
      }
      Some(Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event))))
    case FromProvider(NsiRequesterMessage(_, timeout: MessageDeliveryTimeout)) =>
      val event = MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
        .withConnectionId(id)
        .withNotificationId(newNotificationId())
        .withCorrelationId(timeout.timeout.getCorrelationId())
        .withTimeStamp(timeout.timeout.getTimeStamp()))
      Some(Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event))))
    case failure: MessageDeliveryFailure =>
      val event = MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
        .withConnectionId(id)
        .withNotificationId(newNotificationId)
        .withCorrelationId(failure.correlationId.toString)
        .withTimeStamp(failure.timestamp.toXmlGregorianCalendar))
      Some(Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event))))
    case _ =>
      None
  }

  def query = {
    val children = rsm.childConnections.zipWithIndex.collect {
      case ((segment, Some(id)), order) => new ChildSummaryType().
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
    case (segment, id) => ConnectionData(
      id,
      segment.provider.nsa,
      id.map(rsm.childConnectionState).getOrElse(ReservationStateEnumType.RESERVE_CHECKING),
      id.flatMap(id => lsm.map(_.childConnectionState(id))).getOrElse(LifecycleStateEnumType.CREATED),
      id.flatMap(id => psm.map(_.childConnectionState(id))).getOrElse(ProvisionStateEnumType.RELEASED),
      id.flatMap(id => dsm.map(_.childConnectionState(id))).getOrElse(false),
      id.flatMap(mostRecentChildExceptions.get))
  }.toSeq

  def notifications: Seq[NotificationBaseType] = nsiNotifications.map {
    case ErrorEvent(event) => event
    case DataPlaneStateChange(event) => event
    case ReserveTimeout(event) => event
    case MessageDeliveryTimeout(event) => event
  }

}
