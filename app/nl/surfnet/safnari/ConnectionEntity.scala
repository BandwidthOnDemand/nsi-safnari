package nl.surfnet.safnari

import akka.actor._
import java.net.URI
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import play.api.Logger
import scala.math.Ordering.Implicits._
import scala.util.Try
import java.util.concurrent.atomic.AtomicInteger

class ConnectionEntity(val id: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve], newCorrelationId: () => CorrelationId, val aggregatorNsa: String, pathComputationAlgorithm: PathComputationAlgorithm, nsiReplyToUri: URI, pceReplyUri: URI) {
  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newNsiHeaders(provider: ProviderEndPoint) = NsiHeaders(newCorrelationId(), aggregatorNsa, provider.nsa, Some(nsiReplyToUri), NsiHeaders.ProviderProtocolVersion, initialReserve.headers.sessionSecurityAttrs, Nil)
  private def newInitialReserveNsiHeaders(provider: ProviderEndPoint) = {
    val oldTrace = initialReserve.headers.connectionTrace
    val index = if (oldTrace.isEmpty) 0 else oldTrace.map(_.getIndex()).max + 1
    val newTrace = new ConnectionType().withIndex(index).withValue(s"$aggregatorNsa:$id") :: initialReserve.headers.connectionTrace
    newNsiHeaders(provider).copy(connectionTrace = newTrace)
  }
  private def newNotifyHeaders() = NsiHeaders(newCorrelationId(), requesterNSA, aggregatorNsa, None, NsiHeaders.RequesterProtocolVersion, Nil, Nil)
  private var nextNotificationId = new AtomicInteger(1)
  private var nextResultId = new AtomicInteger(1)
  private var nsiNotifications: List[NsiNotification] = Nil
  private var nsiResults: List[QueryResultResponseType] = Nil
  private def newNotificationId() = nextNotificationId.getAndIncrement()
  private def newResultId() = nextResultId.getAndIncrement()
  private var mostRecentChildExceptions = Map.empty[ConnectionId, ServiceExceptionType]
  private var committedCriteria: Option[ReservationConfirmCriteriaType] = None

  val rsm = new ReservationStateMachine(id, initialReserve, pceReplyUri, newCorrelationId, newNsiHeaders, newInitialReserveNsiHeaders, newNotificationId, pathComputationAlgorithm, { error =>
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
        case Left(connectionIds)         => connectionIds.contains(id)
        case Right(globalReservationIds) => globalReservationIds.contains(globalReservationId)
      })

      val done = rsm.childConnections.forall { case (_, _, connectionId) => connectionId.isDefined }

      val qrsm = new QueryRecursiveStateMachine(
        id,
        pm.asInstanceOf[NsiProviderMessage[QueryRecursive]],
        initialReserve,
        connectionStates,
        rsm.childConnections.map { case (segment, _, id) => segment.provider -> id }.toMap,
        newCorrelationId,
        newNsiHeaders)

      val output = qrsm.process(message)

      output foreach (registerProviderConversations(_, qrsm))

      output
  }

  def queryRecursiveResult(message: FromProvider): Option[Seq[OutboundMessage]] = message match {
    case FromProvider(NsiRequesterMessage(_, _: QueryRecursiveConfirmed)) | FromProvider(NsiRequesterMessage(_, _: Error)) =>
      val qrsm = providerConversations.get(message.correlationId)
      providerConversations -= message.correlationId

      qrsm.flatMap(_.process(message))
  }

  def process(message: InboundMessage): Either[ServiceExceptionType, Seq[OutboundMessage]] = {
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
      case AckFromPce(_) => Some(rsm)

      case FromProvider(NsiRequesterMessage(_, _: ReserveTimeout)) => Some(rsm)
      case FromProvider(NsiRequesterMessage(_, _: DataPlaneStateChange)) => dsm
      case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) if error.error.getEvent() == EventEnumType.FORCED_END =>
        lsm
      case FromProvider(NsiRequesterMessage(_, _: ErrorEvent))             => None
      case FromProvider(NsiRequesterMessage(_, _: MessageDeliveryTimeout)) => None

      case FromProvider(NsiRequesterMessage(headers, _)) =>
        val stateMachine = providerConversations.get(headers.correlationId)
        if (stateMachine.isEmpty) Logger.debug(s"No active conversation for reply ${message.toShortString}, one of ${providerConversations.keySet.mkString(", ")} expected")
        providerConversations -= headers.correlationId
        stateMachine

      case AckFromProvider(NsiProviderMessage(headers, _)) =>
        val stateMachine = providerConversations.get(headers.correlationId)
        if (stateMachine.isEmpty) Logger.debug(s"No active conversation for ack ${message.toShortString}")
        stateMachine

      case _: MessageDeliveryFailure =>
        None

      case message: PassedEndTime =>
        /*
         * Only accept PassedEndTime messages that are at or after the scheduled end time.
         * In some (rare) cases it may be possible to receive a scheduled PassedEndTime message
         * earlier (e.g. when the end time is modified just as the scheduler sends a
         * PassedEndTime message).
         */
        for {
          lifecycleStateMachine <- lsm
          scheduledEndTime <- rsm.criteria.getSchedule().endTime
          if scheduledEndTime <= DateTime.now()
        } yield lifecycleStateMachine
    }

    stateMachine.flatMap(applyMessageToStateMachine(_, message)).orElse(handleUnhandledProviderNotifications(message)).toRight(messageNotApplicable(message))
  }

  def process(message: OutboundMessage): Unit = message match {
    case ToRequester(NsiRequesterMessage(headers, notification: NsiNotification)) =>
      nsiNotifications = notification :: nsiNotifications
    case ToRequester(NsiRequesterMessage(headers, result: NsiCommandReply)) =>
      def genericConfirmed(connectionId: ConnectionId) = new GenericConfirmedType().withConnectionId(connectionId)

      val nsiResult = (result match {
        case ReserveConfirmed(connectionId, criteria) => new QueryResultResponseType().withReserveConfirmed(new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria))
        case ReserveCommitConfirmed(connectionId)     => new QueryResultResponseType().withReserveCommitConfirmed(genericConfirmed(connectionId))
        case ReleaseConfirmed(connectionId)           => new QueryResultResponseType().withReleaseConfirmed(genericConfirmed(connectionId))
        case ReserveFailed(failed)                    => new QueryResultResponseType().withReserveFailed(failed)
        case ProvisionConfirmed(connectionId)         => new QueryResultResponseType().withProvisionConfirmed(genericConfirmed(connectionId))
        case ReserveAbortConfirmed(connectionId)      => new QueryResultResponseType().withReserveAbortConfirmed(genericConfirmed(connectionId))
        case ReserveCommitFailed(failed)              => new QueryResultResponseType().withReserveCommitFailed(failed)
        case TerminateConfirmed(connectionId)         => new QueryResultResponseType().withTerminateConfirmed(genericConfirmed(connectionId))
      }).withCorrelationId(headers.correlationId.toString()).withResultId(newResultId()).withTimeStamp(DateTime.now().toXmlGregorianCalendar)
      nsiResults = nsiResult :: nsiResults
    case _ =>
  }

  private def applyMessageToStateMachine(stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage], message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val output = stateMachine.process(message)

    output.foreach { messages =>
      registerProviderConversations(messages, stateMachine)

      messages.collectFirst {
        case ToRequester(NsiRequesterMessage(_, _: ReserveCommitConfirmed)) =>
          val children = rsm.childConnections.map {
            case (segment, _, Some(connectionId)) => connectionId -> segment.provider
            case (segment, _, None)               => throw new IllegalStateException(s"reserveConfirmed with unknown child connectionId for $segment")
          }.toMap
          committedCriteria = Some(initialReserve.body.criteria)
          otherStateMachines = Some((
            new ProvisionStateMachine(id, newNsiHeaders, children),
            new LifecycleStateMachine(id, newNsiHeaders, newNotifyHeaders, newNotificationId, rsm.children),
            new DataPlaneStateMachine(id, newNotifyHeaders, newNotificationId, children)))
      }
    }

    output
  }

  private def messageNotApplicable(message: InboundMessage): ServiceExceptionType = NsiError.InvalidTransition.toServiceException(aggregatorNsa)

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
        .withOriginatingConnectionId(error.error.getOriginatingConnectionId())
        .withOriginatingNSA(error.error.getOriginatingNSA())
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
        .withCorrelationId(failure.originalCorrelationId.toString)
        .withTimeStamp(failure.timestamp.toXmlGregorianCalendar))
      Some(Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event))))
    case _ =>
      None
  }

  def query = {
    lazy val children = rsm.childConnections.zipWithIndex.collect {
      case ((segment, _, Some(id)), order) => new ChildSummaryType()
        .withConnectionId(id)
        .withProviderNSA(segment.provider.nsa)
        .withServiceType(segment.serviceType.serviceType)
        .withPointToPointService(segment.serviceType.service)
        .withOrder(order)
    }

    val result = new QuerySummaryResultType()
      .withGlobalReservationId(initialReserve.body.body.getGlobalReservationId())
      .withDescription(initialReserve.body.body.getDescription())
      .withConnectionId(id)
      .withRequesterNSA(requesterNSA)
      .withConnectionStates(connectionStates)

    committedCriteria.foreach { criteria =>
      result.getCriteria().add(new QuerySummaryResultCriteriaType()
        .withVersion(criteria.getVersion())
        .withSchedule(criteria.getSchedule())
        .withServiceType(criteria.getServiceType())
        .withPointToPointService(initialReserve.body.service)
        .withChildren(new ChildSummaryListType().withChild(children: _*))
        .tap(_.getOtherAttributes().putAll(criteria.getOtherAttributes())))
    }

    result
  }

  def connectionStates: ConnectionStatesType = {
    val version = rsm.version
    new ConnectionStatesType()
      .withReservationState(rsm.reservationState)
      .withProvisionState(psm.map(_.provisionState).getOrElse(ProvisionStateEnumType.RELEASED))
      .withLifecycleState(lsm.map(_.lifecycleState).getOrElse(LifecycleStateEnumType.CREATED))
      .withDataPlaneStatus(dsm.map(_.dataPlaneStatus(version)).getOrElse(new DataPlaneStatusType()))
  }

  def segments: Seq[ConnectionData] = rsm.childConnections.map {
    case (segment, correlationId, id) => ConnectionData(
      id,
      segment.provider.nsa,
      rsm.childConnectionStateByInitialCorrelationId(correlationId),
      id.flatMap(id => lsm.map(_.childConnectionState(id))).getOrElse(LifecycleStateEnumType.CREATED),
      id.flatMap(id => psm.map(_.childConnectionState(id))).getOrElse(ProvisionStateEnumType.RELEASED),
      id.flatMap(id => dsm.map(_.childConnectionState(id))).getOrElse(false),
      id.flatMap(mostRecentChildExceptions.get))
  }.toSeq

  def notifications: Seq[NotificationBaseType] = nsiNotifications.map {
    case ErrorEvent(event)             => event
    case DataPlaneStateChange(event)   => event
    case ReserveTimeout(event)         => event
    case MessageDeliveryTimeout(event) => event
  }

  def results: Seq[QueryResultResponseType] = nsiResults

}
