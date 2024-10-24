/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package nl.surfnet.safnari

import java.net.URI
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import javax.xml.namespace.QName

import nl.surfnet.nsiv2.messages.{given, *}
import nl.surfnet.nsiv2.utils.*

import org.ogf.schemas.nsi._2013._12.connection.types.*
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import play.api.Logger

import scala.math.Ordering.Implicits.*
import scala.util.Try

case class ConnectionContext(
    clock: java.time.Clock
)

class ConnectionEntity(
    val aggregatorNsa: String,
    val id: ConnectionId,
    initialReserve: NsiProviderMessage[InitialReserve],
    newCorrelationId: () => CorrelationId,
    defaultPathComputationAlgorithm: PathComputationAlgorithm,
    nsiReplyToUri: URI,
    pceReplyUri: URI
):
  private final val PATH_COMPUTATION_ALGORITHM_PARAMETER_TYPE = "pathComputationAlgorithm"
  private val logger = Logger(classOf[ConnectionEntity])

  private def requesterNSA = initialReserve.headers.requesterNSA
  private def newRequestHeaders(
      requesterMessage: NsiProviderMessage[NsiProviderOperation],
      provider: ProviderEndPoint
  ) = requesterMessage.headers.copy(
    correlationId = newCorrelationId(),
    requesterNSA = aggregatorNsa,
    providerNSA = provider.nsa,
    replyTo = Some(nsiReplyToUri),
    protocolVersion = NsiHeaders.ProviderProtocolVersion
  )

  private def newNotifyHeaders() = NsiHeaders(
    newCorrelationId(),
    requesterNSA,
    aggregatorNsa,
    None,
    NsiHeaders.RequesterProtocolVersion,
    Nil
  )
  private val nextNotificationId = new AtomicInteger(1)
  private val nextResultId = new AtomicInteger(1)
  private var nsiNotifications: List[NsiNotification] = Nil
  private var nsiResults: List[QueryResultResponseType] = Nil
  private def newNotificationId() = nextNotificationId.getAndIncrement()
  private def newResultId() = nextResultId.getAndIncrement()
  private var mostRecentChildExceptions = Map.empty[ConnectionId, ServiceExceptionType]

  var lastUpdatedAt: Instant = Instant.ofEpochSecond(0);

  var children: ChildConnectionIds = ChildConnectionIds()

  private var pathComputationAlgorithm = initialReserve.body.service
    .flatMap(
      _.parameters(PATH_COMPUTATION_ALGORITHM_PARAMETER_TYPE)
        .flatMap(PathComputationAlgorithm.fromString)
    )
    .getOrElse(defaultPathComputationAlgorithm)

  val rsm = new ReservationStateMachine(
    aggregatorNsa,
    id,
    initialReserve,
    pceReplyUri,
    children,
    newCorrelationId,
    newRequestHeaders,
    newNotificationId _,
    newNotifyHeaders _,
    pathComputationAlgorithm,
    { error =>
      new GenericFailedType()
        .withConnectionId(id)
        .withConnectionStates(connectionStates)
        .withServiceException(error.toServiceException(aggregatorNsa))
    }
  )
  val lsm = new LifecycleStateMachine(
    id,
    newRequestHeaders,
    newNotifyHeaders _,
    newNotificationId _,
    children
  )

  private val globalReservationId: Option[GlobalReservationId] = Try(
    URI.create(initialReserve.body.body.getGlobalReservationId())
  ).toOption

  private var otherStateMachines: Option[(ProvisionStateMachine, DataPlaneStateMachine)] = None
  def psm: Option[ProvisionStateMachine] = otherStateMachines.map(_._1)
  def dsm: Option[DataPlaneStateMachine] = otherStateMachines.map(_._2)

  private var providerConversations
      : Map[CorrelationId, FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = Map.empty

  def queryRecursive(message: FromRequester): Option[Seq[OutboundMessage]] = message match
    case FromRequester(pm @ NsiProviderMessage(_, QueryRecursive(ids, ifModifiedSince))) =>
      require(ids.fold(true) {
        case Left(connectionIds)         => connectionIds.contains(id)
        case Right(globalReservationIds) => globalReservationIds.contains(globalReservationId)
      })

      val qrsm = new QueryRecursiveStateMachine(
        id,
        pm.asInstanceOf[NsiProviderMessage[QueryRecursive]],
        initialReserve,
        connectionStates,
        children.childConnections.map { case (segment, _, id) => segment.provider -> id }.toMap,
        newCorrelationId,
        newRequestHeaders(pm, _),
        ifModifiedSince
      )

      val output = qrsm.process(message)

      output foreach (registerProviderConversations(_, qrsm))

      output
    case _ => throw RuntimeException(s"unexpected query $message")

  def queryRecursiveResult(message: FromProvider): Option[Seq[OutboundMessage]] = message match
    case FromProvider(NsiRequesterMessage(_, QueryRecursiveConfirmed(_))) | FromProvider(
          NsiRequesterMessage(_, ErrorReply(_))
        ) =>
      val qrsm = providerConversations.get(message.correlationId)
      providerConversations -= message.correlationId

      qrsm.flatMap(_.process(message))
    case _ => throw RuntimeException(s"unexpected query result $message")

  def process(
      message: InboundMessage
  )(context: ConnectionContext): Either[ServiceExceptionType, Seq[OutboundMessage]] =
    children = children.update(message, newCorrelationId)

    message match
      case AckFromProvider(NsiProviderMessage(_, ServiceException(exception))) =>
        Option(exception.getConnectionId()).foreach { connectionId =>
          mostRecentChildExceptions += connectionId -> exception;
        }
      case failed: MessageDeliveryFailure =>
        failed.connectionId.foreach { connectionId =>
          val child = children.childrenByConnectionId.get(connectionId)
          val childNsaId = child.map(_.nsa)
          mostRecentChildExceptions += connectionId -> NsiError.MessageDeliveryError
            .withVariables(
              NsiHeaders.PROVIDER_NSA -> childNsaId.getOrElse(""),
              new QName("error") -> failed.toShortString
            )
            .toServiceException(childNsaId.getOrElse(""))
        }
      case _ =>

    if lsm.lifecycleState == LifecycleStateEnumType.TERMINATED then Left(messageNotApplicable)
    else
      val outputs = stateMachines(message, context).flatMap { stateMachine =>
        applyMessageToStateMachine(stateMachine, message)
      }

      if outputs.isEmpty then
        handleUnhandledProviderNotifications(message).toRight(messageNotApplicable)
      else
        lastUpdatedAt = context.clock.instant

        Right(outputs.flatten)
  end process

  def process(message: OutboundMessage)(context: ConnectionContext): Unit = message match
    case ToPce(request: PathComputationRequest) =>
      pathComputationAlgorithm = request.algorithm
    case ToRequester(NsiRequesterMessage(_, notification: NsiNotification)) =>
      nsiNotifications = notification :: nsiNotifications
    case ToRequester(NsiRequesterMessage(headers, result: NsiCommandReply)) =>
      def genericConfirmed(connectionId: ConnectionId) =
        new GenericConfirmedType().withConnectionId(connectionId)

      val nsiResult = (result match
        case ReserveConfirmed(connectionId, criteria) =>
          new QueryResultResponseType().withReserveConfirmed(
            new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria)
          )
        case ReserveCommitConfirmed(connectionId) =>
          new QueryResultResponseType().withReserveCommitConfirmed(genericConfirmed(connectionId))
        case ReleaseConfirmed(connectionId) =>
          new QueryResultResponseType().withReleaseConfirmed(genericConfirmed(connectionId))
        case ReserveFailed(failed) => new QueryResultResponseType().withReserveFailed(failed)
        case ProvisionConfirmed(connectionId) =>
          new QueryResultResponseType().withProvisionConfirmed(genericConfirmed(connectionId))
        case ReserveAbortConfirmed(connectionId) =>
          new QueryResultResponseType().withReserveAbortConfirmed(genericConfirmed(connectionId))
        case ReserveCommitFailed(failed) =>
          new QueryResultResponseType().withReserveCommitFailed(failed)
        case TerminateConfirmed(connectionId) =>
          new QueryResultResponseType().withTerminateConfirmed(genericConfirmed(connectionId))
      )
      .withCorrelationId(headers.correlationId.toString())
        .withResultId(newResultId())
        .withTimeStamp(Instant.now(context.clock).toXMLGregorianCalendar())
      nsiResults = nsiResult :: nsiResults
    case _ =>

  private def stateMachines(
      message: InboundMessage,
      context: ConnectionContext
  ): List[FiniteStateMachine[_, _, InboundMessage, OutboundMessage]] = message match
    case FromRequester(NsiProviderMessage(_, _: InitialReserve)) => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: ModifyReserve))  => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: ReserveCommit))  => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: ReserveAbort))   => List(rsm)
    case FromRequester(NsiProviderMessage(_, _: Provision))      => psm.toList
    case FromRequester(NsiProviderMessage(_, _: Release))        => psm.toList
    case FromRequester(NsiProviderMessage(_, _: Terminate))      => List(lsm)
    case FromRequester(NsiProviderMessage(_, _))                 => Nil

    case FromPce(_)    => List(rsm)
    case AckFromPce(_) => List(rsm)

    case FromProvider(NsiRequesterMessage(_, _: DataPlaneStateChange)) => dsm.toList
    case FromProvider(NsiRequesterMessage(_, error: ErrorEvent))
        if error.notification.getEvent() == EventEnumType.FORCED_END =>
      List(lsm)
    case FromProvider(NsiRequesterMessage(_, _: ErrorEvent))             => Nil
    case FromProvider(NsiRequesterMessage(_, _: ReserveTimeout))         => List(rsm)
    case FromProvider(NsiRequesterMessage(_, _: MessageDeliveryTimeout)) => Nil

    case FromProvider(NsiRequesterMessage(headers, body)) =>
      val stateMachine = providerConversations.get(headers.correlationId)
      providerConversations -= headers.correlationId
      stateMachine match
        case None =>
          logger.debug(
            s"No active conversation for reply ${message.toShortString}, one of [${providerConversations.keySet.mkString(", ")}] expected"
          )
          Nil
        case Some(sm) =>
          List(sm) ++ (body match
            case _: ReserveConfirmed | _: ReserveFailed => List(lsm)
            case _                                      => Nil
          )

    case AckFromProvider(NsiProviderMessage(headers, body)) =>
      val stateMachine = providerConversations.get(headers.correlationId)
      if stateMachine.isEmpty then
        logger.debug(s"No active conversation for ack ${message.toShortString}")
      stateMachine match
        case None =>
          logger.debug(
            s"No active conversation for ack ${message.toShortString}, one of [${providerConversations.keySet.mkString(", ")}] expected"
          )
          Nil
        case Some(sm) =>
          List(sm) ++ (body match
            case _: ReserveResponse => List(lsm)
            case _                  => Nil
          )

    case _: MessageDeliveryFailure =>
      Nil

    case _: PassedEndTime =>
      /*
       * Only accept PassedEndTime messages that are at or after the scheduled end time.
       * In some (rare) cases it may be possible to receive a scheduled PassedEndTime message
       * earlier (e.g. when the end time is modified just as the scheduler sends a
       * PassedEndTime message).
       */
      if rsm.committedCriteria
          .flatMap(_.getSchedule().endTime.toOption(None))
          .exists(_ <= Instant.now(context.clock))
      then List(lsm)
      else Nil

  private def applyMessageToStateMachine(
      stateMachine: FiniteStateMachine[?, ?, InboundMessage, OutboundMessage],
      message: InboundMessage
  ): Option[Seq[OutboundMessage]] =
    val output = stateMachine.process(message)
    output.foreach { messages =>
      registerProviderConversations(messages, stateMachine)

      otherStateMachines = otherStateMachines orElse messages.collectFirst {
        case ToRequester(NsiRequesterMessage(_, _: ReserveCommitConfirmed)) =>
          val children = this.children.childConnections.map {
            case (segment, _, Present(connectionId)) => connectionId -> segment.provider
            case (segment, _, (Pending | Never)) =>
              throw new IllegalStateException(
                s"reserveConfirmed with unknown child connectionId for $segment"
              )
          }.toMap

          (
            new ProvisionStateMachine(id, newRequestHeaders, children),
            new DataPlaneStateMachine(id, newNotifyHeaders _, newNotificationId _, children)
          )
      }
    }

    output
  end applyMessageToStateMachine

  private lazy val messageNotApplicable: ServiceExceptionType =
    NsiError.InvalidTransition.toServiceException(aggregatorNsa)

  private def registerProviderConversations(
      messages: Seq[OutboundMessage],
      stateMachine: FiniteStateMachine[?, ?, InboundMessage, OutboundMessage]
  ): Unit =
    providerConversations ++= messages.collect { case message: ToProvider =>
      logger.trace(s"Registering conversation for ${message.toShortString}")
      message.correlationId -> stateMachine
    }

  private def handleUnhandledProviderNotifications(
      message: InboundMessage
  ): Option[Seq[OutboundMessage]] =
    val eventOption: Option[NsiNotification] = Some(message).collect {
      case FromProvider(NsiRequesterMessage(_, error: ErrorEvent)) =>
        val event = ErrorEvent(
          new ErrorEventType()
            .withConnectionId(id)
            .withNotificationId(newNotificationId())
            .withTimeStamp(error.notification.getTimeStamp())
            .withEvent(error.notification.getEvent())
            .withOriginatingConnectionId(error.notification.getOriginatingConnectionId())
            .withOriginatingNSA(error.notification.getOriginatingNSA())
            .withAdditionalInfo(error.notification.getAdditionalInfo())
        )
        if error.notification.getServiceException() ne null then
          event.notification.withServiceException(
            new ServiceExceptionType()
              .withConnectionId(id)
              .withNsaId(aggregatorNsa)
              .withErrorId(error.notification.getServiceException().getErrorId())
              .withText(error.notification.getServiceException().getText())
              .withServiceType(
                error.notification.getServiceException().getServiceType()
              ) // FIXME own service type?
              .withChildException(error.notification.getServiceException())
          )
        event
      case FromProvider(NsiRequesterMessage(_, timeout: MessageDeliveryTimeout)) =>
        MessageDeliveryTimeout(
          new MessageDeliveryTimeoutRequestType()
            .withConnectionId(id)
            .withNotificationId(newNotificationId())
            .withCorrelationId(timeout.notification.getCorrelationId())
            .withTimeStamp(timeout.notification.getTimeStamp())
        )
      case failure: MessageDeliveryFailure =>
        MessageDeliveryTimeout(
          new MessageDeliveryTimeoutRequestType()
            .withConnectionId(id)
            .withNotificationId(newNotificationId())
            .withCorrelationId(failure.originalCorrelationId.toString)
            .withTimeStamp(failure.timestamp.toXMLGregorianCalendar())
        )
    }

    eventOption.map { event =>
      Seq(ToRequester(NsiRequesterMessage(newNotifyHeaders(), event)))
    }
  end handleUnhandledProviderNotifications

  private def childP2PServiceType(
      correlationId: CorrelationId,
      segment: ComputedSegment
  ): P2PServiceBaseType =
    val childConnectionCriteria = rsm.childConnectionCriteria(correlationId)

    val confirmCriteria =
      childConnectionCriteria.committed.orElse(childConnectionCriteria.confirmed)

    confirmCriteria
      .flatMap(_.pointToPointService)
      .orElse(childConnectionCriteria.requested.flatMap(_.pointToPointService))
      .getOrElse(segment.serviceType.service)

  def query: QuerySummaryResultType =
    val result = new QuerySummaryResultType()
      .withGlobalReservationId(initialReserve.body.body.getGlobalReservationId())
      .withDescription(initialReserve.body.body.getDescription())
      .withConnectionId(id)
      .withRequesterNSA(requesterNSA)
      .withConnectionStates(connectionStates)

    rsm.committedCriteria.foreach { criteria =>
      val children = this.children.childConnections.zipWithIndex.collect {
        case ((segment, correlationId, Present(childConnectionId)), order) =>
          val p2ps = childP2PServiceType(correlationId, segment)
          new ChildSummaryType()
            .withConnectionId(childConnectionId)
            .withProviderNSA(segment.provider.nsa)
            .withServiceType(segment.serviceType.serviceType)
            .withPointToPointService(p2ps)
            .withOrder(order)
      }

      result
        .getCriteria()
        .add(
          new QuerySummaryResultCriteriaType()
            .withVersion(criteria.getVersion())
            .withSchedule(criteria.getSchedule())
            .withServiceType(criteria.getServiceType())
            .withPointToPointService(criteria.pointToPointService.get)
            .withChildren(new ChildSummaryListType().withChild(children*))
            .tap(_.getOtherAttributes().putAll(criteria.getOtherAttributes()))
        )
    }

    result
  end query

  def connectionStates: ConnectionStatesType =
    new ConnectionStatesType()
      .withReservationState(rsm.reservationState)
      .withLifecycleState(lsm.lifecycleState)
      .withProvisionState(psm.map(_.provisionState).getOrElse(ProvisionStateEnumType.RELEASED))
      .withDataPlaneStatus(dsm.map(_.dataPlaneStatus).getOrElse(new DataPlaneStatusType()))

  def segments: Seq[ConnectionData] = children.childConnections.map {
    case (segment, correlationId, id) =>
      val p2ps = childP2PServiceType(correlationId, segment)

      ConnectionData(
        id,
        segment.provider.nsa,
        p2ps.getSourceSTP(),
        p2ps.getDestSTP(),
        p2ps.getEro(),
        rsm.childConnectionStateByInitialCorrelationId(correlationId),
        id.map(id => lsm.childConnectionState(id))
          .present
          .getOrElse(LifecycleStateEnumType.CREATED),
        id.present
          .flatMap(id => psm.map(_.childConnectionState(id)))
          .getOrElse(ProvisionStateEnumType.RELEASED),
        id.present
          .flatMap(id => dsm.map(_.childConnectionState(id)))
          .getOrElse(new DataPlaneStatusType()),
        id.present.flatMap(mostRecentChildExceptions.get)
      )
  }.toSeq

  def notifications: Seq[NotificationBaseType] = nsiNotifications.map {
    case ErrorEvent(event)             => event
    case DataPlaneStateChange(event)   => event
    case ReserveTimeout(event)         => event
    case MessageDeliveryTimeout(event) => event
  }

  def results: Seq[QueryResultResponseType] = nsiResults
end ConnectionEntity
