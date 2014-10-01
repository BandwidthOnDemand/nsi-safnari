package nl.surfnet.safnari

import java.net.URI
import java.util.concurrent.atomic.AtomicInteger

import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import play.api.Logger

import scala.math.Ordering.Implicits._
import scala.util.Try

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

  var children = ChildConnectionIds()

  val rsm = new ReservationStateMachine(id, initialReserve, pceReplyUri, children, newCorrelationId, newNsiHeaders, newInitialReserveNsiHeaders, newNotificationId, pathComputationAlgorithm, { error =>
    new GenericFailedType().
      withConnectionId(id).
      withConnectionStates(connectionStates).
      withServiceException(error.toServiceException(aggregatorNsa))
  })
  val lsm = new LifecycleStateMachine(id, newNsiHeaders, newNotifyHeaders, newNotificationId, children)

  private val globalReservationId: Option[GlobalReservationId] = Try(URI.create(initialReserve.body.body.getGlobalReservationId())).toOption

  private var otherStateMachines: Option[(ProvisionStateMachine, DataPlaneStateMachine)] = None
  def psm = otherStateMachines.map(_._1)
  def dsm = otherStateMachines.map(_._2)

  private var providerConversations: Map[CorrelationId, FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = Map.empty

  def queryRecursive(message: FromRequester): Option[Seq[OutboundMessage]] = message match {
    case FromRequester(pm @ NsiProviderMessage(_, QueryRecursive(ids))) =>
      require(ids.fold(true) {
        case Left(connectionIds)         => connectionIds.contains(id)
        case Right(globalReservationIds) => globalReservationIds.contains(globalReservationId)
      })

      val done = children.childConnections.forall { case (_, _, connectionId) => connectionId.isDefined }

      val qrsm = new QueryRecursiveStateMachine(
        id,
        pm.asInstanceOf[NsiProviderMessage[QueryRecursive]],
        initialReserve,
        connectionStates,
        children.childConnections.map { case (segment, _, id) => segment.provider -> id }.toMap,
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
    children = children.update(message, newCorrelationId)

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

    if (lsm.lifecycleState == LifecycleStateEnumType.TERMINATED) {
      Left(messageNotApplicable(message))
    } else {
      val outputs = stateMachines(message).flatMap { stateMachine =>
        applyMessageToStateMachine(stateMachine, message)
      }

      if (outputs.isEmpty)
        handleUnhandledProviderNotifications(message).toRight(messageNotApplicable(message))
      else
        Right(outputs.flatten)
    }
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

  private def stateMachines(message: InboundMessage): List[FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = message match {
    case FromRequester(NsiProviderMessage(_, _: InitialReserve))       => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: ReserveCommit))        => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: ReserveAbort))         => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: Provision))            => psm.toList
    case FromRequester(NsiProviderMessage(_, _: Release))              => psm.toList
    case FromRequester(NsiProviderMessage(_, _: Terminate))            => List(lsm)
    case FromRequester(NsiProviderMessage(_, _))                       => Nil

    case FromPce(_)                                                    => List(rsm)
    case AckFromPce(_)                                                 => List(rsm)

    case FromProvider(NsiRequesterMessage(_, _: DataPlaneStateChange)) => dsm.toList
    case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) if error.notification.getEvent() == EventEnumType.FORCED_END =>
      List(lsm)
    case FromProvider(NsiRequesterMessage(_, _: ReserveTimeout))         => Nil
    case FromProvider(NsiRequesterMessage(_, _: ErrorEvent))             => Nil
    case FromProvider(NsiRequesterMessage(_, _: MessageDeliveryTimeout)) => Nil

    case FromProvider(NsiRequesterMessage(headers, body)) =>
      val stateMachine = providerConversations.get(headers.correlationId)
      providerConversations -= headers.correlationId
      stateMachine match {
        case None =>
          Logger.debug(s"No active conversation for reply ${message.toShortString}, one of [${providerConversations.keySet.mkString(", ")}] expected")
          Nil
        case Some(sm) =>
          List(sm) ++ (body match {
            case _: ReserveConfirmed | _: ReserveFailed => List(lsm)
            case _                                      => Nil
          })
      }

    case AckFromProvider(NsiProviderMessage(headers, body)) =>
      val stateMachine = providerConversations.get(headers.correlationId)
      if (stateMachine.isEmpty) Logger.debug(s"No active conversation for ack ${message.toShortString}")
      stateMachine match {
        case None =>
          Logger.debug(s"No active conversation for ack ${message.toShortString}, one of [${providerConversations.keySet.mkString(", ")}] expected")
          Nil
        case Some(sm) =>
          List(sm) ++ (body match {
            case _: ReserveResponse => List(lsm)
            case _                  => Nil
          })
      }

    case _: MessageDeliveryFailure =>
      Nil

    case message: PassedEndTime =>
      /*
         * Only accept PassedEndTime messages that are at or after the scheduled end time.
         * In some (rare) cases it may be possible to receive a scheduled PassedEndTime message
         * earlier (e.g. when the end time is modified just as the scheduler sends a
         * PassedEndTime message).
         */
      if (rsm.criteria.getSchedule().endTime.exists(_ <= DateTime.now)) List(lsm) else Nil
  }

  private def applyMessageToStateMachine(stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage], message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val output = stateMachine.process(message)
    output.foreach { messages =>
      registerProviderConversations(messages, stateMachine)

      messages.collectFirst {
        case ToRequester(NsiRequesterMessage(_, _: ReserveCommitConfirmed)) =>
          val children = this.children.childConnections.map {
            case (segment, _, Some(connectionId)) => connectionId -> segment.provider
            case (segment, _, None)               => throw new IllegalStateException(s"reserveConfirmed with unknown child connectionId for $segment")
          }.toMap
          committedCriteria = Some(initialReserve.body.criteria)
          otherStateMachines = Some((
            new ProvisionStateMachine(id, newNsiHeaders, children),
            new DataPlaneStateMachine(id, newNotifyHeaders, newNotificationId, currentVersion, children)))
      }
    }

    output
  }

  private def currentVersion() = rsm.version

  private def messageNotApplicable(message: InboundMessage): ServiceExceptionType = NsiError.InvalidTransition.toServiceException(aggregatorNsa)

  private def registerProviderConversations(messages: Seq[OutboundMessage], stateMachine: FiniteStateMachine[_, _, InboundMessage, OutboundMessage]): Unit = {
    providerConversations ++= messages.collect {
      case message: ToProvider =>
        Logger.trace(s"Registering conversation for ${message.toShortString}")
        message.correlationId -> stateMachine
    }
  }

  private def handleUnhandledProviderNotifications(message: InboundMessage): Option[Seq[OutboundMessage]] = {
    val eventOption: Option[NsiNotification] = Some(message).collect {
      case FromProvider(NsiRequesterMessage(_, message: ReserveTimeout)) =>
        ReserveTimeout(new ReserveTimeoutRequestType()
          .withConnectionId(id)
          .withNotificationId(newNotificationId())
          .withTimeStamp(message.notification.getTimeStamp())
          .withTimeoutValue(message.notification.getTimeoutValue())
          .withOriginatingConnectionId(message.notification.getOriginatingConnectionId())
          .withOriginatingNSA(message.notification.getOriginatingNSA()))
      case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) =>
        val event = ErrorEvent(new ErrorEventType()
          .withConnectionId(id)
          .withNotificationId(newNotificationId())
          .withTimeStamp(error.notification.getTimeStamp())
          .withEvent(error.notification.getEvent())
          .withOriginatingConnectionId(error.notification.getOriginatingConnectionId())
          .withOriginatingNSA(error.notification.getOriginatingNSA())
          .withAdditionalInfo(error.notification.getAdditionalInfo()))
        if (error.notification.getServiceException() ne null) {
          event.notification.withServiceException(new ServiceExceptionType()
            .withConnectionId(id)
            .withNsaId(aggregatorNsa)
            .withErrorId(error.notification.getServiceException().getErrorId())
            .withText(error.notification.getServiceException().getText())
            .withServiceType(error.notification.getServiceException().getServiceType()) // FIXME own service type?
            .withChildException(error.notification.getServiceException()))
        }
        event
      case FromProvider(NsiRequesterMessage(_, timeout: MessageDeliveryTimeout)) =>
        MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
          .withConnectionId(id)
          .withNotificationId(newNotificationId())
          .withCorrelationId(timeout.notification.getCorrelationId())
          .withTimeStamp(timeout.notification.getTimeStamp()))
      case failure: MessageDeliveryFailure =>
        MessageDeliveryTimeout(new MessageDeliveryTimeoutRequestType()
          .withConnectionId(id)
          .withNotificationId(newNotificationId)
          .withCorrelationId(failure.originalCorrelationId.toString)
          .withTimeStamp(failure.timestamp.toXmlGregorianCalendar))
    }

    eventOption.map { event =>
      Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event)))
    }
  }

  def query = {
    lazy val children = this.children.childConnections.zipWithIndex.collect {
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
    new ConnectionStatesType()
      .withReservationState(rsm.reservationState)
      .withLifecycleState(lsm.lifecycleState)
      .withProvisionState(psm.map(_.provisionState).getOrElse(ProvisionStateEnumType.RELEASED))
      .withDataPlaneStatus(dsm.map(_.dataPlaneStatus).getOrElse(new DataPlaneStatusType()))
  }

  def segments: Seq[ConnectionData] = children.childConnections.map {
    case (segment, correlationId, id) => ConnectionData(
      id,
      segment.provider.nsa,
      segment.serviceType.service.getSourceSTP,
      segment.serviceType.service.getDestSTP,
      rsm.childConnectionStateByInitialCorrelationId(correlationId),
      id.map(id => lsm.childConnectionState(id)).getOrElse(LifecycleStateEnumType.CREATED),
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
