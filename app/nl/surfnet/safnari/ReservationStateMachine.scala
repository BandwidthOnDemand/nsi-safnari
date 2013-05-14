package nl.surfnet.safnari

import akka.actor._
import com.twitter.bijection.Injection
import java.net.URI
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType

class ReservationStateMachine(id: ConnectionId, requesterNSA: String, newCorrelationId: () => CorrelationId, outbound: Message => Unit, pceReplyUri: URI, reservationCommitted: ExistingConnection => Unit)
  extends FiniteStateMachine[ReservationState, Connection](InitialReservationState, NewConnection(id)) {
  when(InitialReservationState) {
    case Event(FromRequester(message: Reserve), _) =>
      val criteria = Injection.invert(message.body.getCriteria())
      goto(PathComputationState) using ExistingConnection(
        id = id,
        reserveCorrelationId = message.correlationId,
        globalReservationId = Option(message.body.getGlobalReservationId()),
        description = Option(message.body.getDescription()),
        criteria = criteria.getOrElse(sys.error("Bad initial reservation criteria"))) replying ReserveResponse(message.correlationId, id)
  }

  when(PathComputationState) {
    case Event(FromPce(message: PathComputationConfirmed), data: ExistingConnection) =>
      val segments = message.segments.map(newCorrelationId() -> _)
      goto(CheckingReservationState) using data.copy(segments = segments.toMap) replying 200
    case Event(FromPce(message: PathComputationFailed), _) =>
      goto(FailedReservationState) replying 200
  }

  when(CheckingReservationState) {
    case Event(FromProvider(message: ReserveConfirmed), data: ExistingConnection) =>
      val newData = data.receivedConnectionId(message.correlationId, message.connectionId, HeldReservationState)
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
    case Event(FromProvider(message: ReserveFailed), data: ExistingConnection) =>
      val newData = data.receivedConnectionId(message.correlationId, message.connectionId, FailedReservationState)
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
  }

  when(HeldReservationState) {
    case Event(FromRequester(commit: ReserveCommit), data: ExistingConnection) =>
      val newData = data.copy(reserveCorrelationId = commit.correlationId, downstreamConnections = data.downstreamConnections.map { _.copy(_2 = CommittingReservationState) })
      goto(CommittingReservationState) using newData replying GenericAck(commit.correlationId)
    case Event(FromRequester(abort: ReserveAbort), data: ExistingConnection) =>
      val newData = data.copy(reserveCorrelationId = abort.correlationId, downstreamConnections = data.downstreamConnections.map { _.copy(_2 = AbortingReservationState) })
      goto(AbortingReservationState) using newData replying GenericAck(abort.correlationId)
  }

  when(CommittingReservationState) {
    case Event(FromProvider(message: ReserveCommitConfirmed), data: ExistingConnection) =>
      val newData = data.copy(downstreamConnections = data.downstreamConnections + (message.connectionId -> ReservedReservationState))
      goto(newData.aggregatedReservationState) using newData replying GenericAck(message.correlationId)
    case Event(FromProvider(message: ReserveCommitFailed), data: ExistingConnection) =>
      val newData = data.copy(downstreamConnections = data.downstreamConnections + (message.connectionId -> CommittingReservationState /* TODO really? */ ))
      stay replying GenericAck(message.correlationId)
  }

  when(AbortingReservationState) {
    case Event(FromProvider(confirmed: ReserveAbortConfirmed), _) => stay replying GenericAck(confirmed.correlationId)
  }

  when(ReservedReservationState) {
    case Event(FromRequester(provision: Provision), _) => stay replying GenericAck(provision.correlationId)
  }
  when(FailedReservationState)(FSM.NullFunction)

  onTransition {
    case InitialReservationState -> PathComputationState =>
      outbound(ToPce(PathComputationRequest(newCorrelationId(), pceReplyUri, nextStateData.asInstanceOf[ExistingConnection].criteria)))

    case PathComputationState -> CheckingReservationState =>
      val data = nextStateData.asInstanceOf[ExistingConnection]
      data.segments.foreach {
        case (correlationId, segment) =>
          val criteria = new ReservationRequestCriteriaType().
            withBandwidth(data.criteria.getBandwidth()).
            withPath(new PathType().withSourceSTP(segment.sourceStp).withDestSTP(segment.destinationStp).withDirectionality(DirectionalityType.BIDIRECTIONAL)).
            withSchedule(data.criteria.getSchedule()).
            withServiceAttributes(data.criteria.getServiceAttributes()).
            withVersion(data.criteria.getVersion())

          val reserveType = new ReserveType().
            withGlobalReservationId(data.globalReservationId.orNull).
            withDescription(data.description.orNull).
            withCriteria(criteria)

          outbound(ToProvider(Reserve(correlationId, reserveType), segment.provider))
      }

    case (PathComputationState | CheckingReservationState) -> FailedReservationState =>
      val failed = new GenericFailedType().withConnectionId(id).withConnectionStates(null).withServiceException(new ServiceExceptionType()
        .withErrorId("0600")
        .withNsaId("urn:ogf:surfnet.nl")
        .withText("Creating reservation is not supported yet"))
      outbound(ToRequester(ReserveFailed(nextStateData.asInstanceOf[ExistingConnection].reserveCorrelationId, failed)))
    case CheckingReservationState -> HeldReservationState =>
      val data = nextStateData.asInstanceOf[ExistingConnection]
      outbound(ToRequester(ReserveConfirmed(data.reserveCorrelationId, id, data.criteria)))
    case HeldReservationState -> CommittingReservationState =>
      val data = nextStateData.asInstanceOf[ExistingConnection]
      data.connections.foreach {
        case (connectionId, correlationId) =>
          val seg = data.segments(correlationId)
          outbound(ToProvider(ReserveCommit(newCorrelationId(), connectionId), seg.provider))
      }
    case HeldReservationState -> AbortingReservationState =>
      val data = nextStateData.asInstanceOf[ExistingConnection]
      data.connections.foreach {
        case (connectionId, correlationId) =>
          val seg = data.segments(correlationId)
          outbound(ToProvider(ReserveAbort(newCorrelationId(), connectionId), seg.provider))
      }
    case CommittingReservationState -> ReservedReservationState =>
      val data = nextStateData.asInstanceOf[ExistingConnection]
      reservationCommitted(data)
      outbound(ToRequester(ReserveCommitConfirmed(data.reserveCorrelationId, id)))
    case ReservedReservationState -> CheckingReservationState => ???
    case FailedReservationState -> AbortingReservationState   => ???
    case AbortingReservationState -> ReservedReservationState => ???
  }

  private def existing = stateData.asInstanceOf[ExistingConnection]
  def globalReservationId = existing.globalReservationId
  def description = existing.description
  def criteria = existing.criteria
  def version = existing.criteria.getVersion()
}
