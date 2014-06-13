package nl.surfnet.safnari

case class ChildConnectionIds(
  segments: Vector[(CorrelationId, ComputedSegment)] = Vector.empty,
  connectionByInitialCorrelationId: Map[CorrelationId, ConnectionId] = Map.empty,
  initialCorrelationIdByConnectionId: Map[ConnectionId, CorrelationId] = Map.empty) {

  def segmentByCorrelationId(correlationId: CorrelationId): ComputedSegment =
    segments.find(_._1 == correlationId).getOrElse(throw new IllegalStateException(s"no computed segment for child correlationId $correlationId"))._2

  def withChildren(segments: Vector[(CorrelationId, ComputedSegment)]) =
    copy(segments = segments)

  def childrenByConnectionId: Map[ConnectionId, ProviderEndPoint] = (for {
    (correlationId, segment) <- segments
    connectionId <- connectionByInitialCorrelationId.get(correlationId)
  } yield connectionId -> segment.provider)(collection.breakOut)

  def awaitingConnectionId: Set[CorrelationId] = initialCorrelationIds -- connectionByInitialCorrelationId.keySet

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId) = {
    copy(
      connectionByInitialCorrelationId = connectionByInitialCorrelationId.updated(correlationId, connectionId),
      initialCorrelationIdByConnectionId = initialCorrelationIdByConnectionId.updated(connectionId, correlationId))
  }

  def childConnections: Seq[(ComputedSegment, CorrelationId, Option[ConnectionId])] = segments.map {
    case (correlationId, segment) =>
      (segment, correlationId, connectionByInitialCorrelationId.get(correlationId))
  }

  def initialCorrelationIdFor(connectionId: ConnectionId): CorrelationId =
    initialCorrelationIdByConnectionId.getOrElse(connectionId, throw new IllegalStateException(s"unknown child connection id $connectionId"))

  def hasConnectionId(initialCorrelationId: CorrelationId): Boolean = connectionByInitialCorrelationId contains initialCorrelationId

  def update(message: InboundMessage, newCorrelationId: () => CorrelationId): ChildConnectionIds = message match {
    case FromPce(message: PathComputationConfirmed) =>
      copy(segments = message.segments.map(newCorrelationId() -> _)(collection.breakOut))
    case AckFromProvider(NsiProviderMessage(headers, ReserveResponse(connectionId))) =>
      receivedConnectionId(headers.correlationId, connectionId)
    case AckFromProvider(NsiProviderMessage(headers, ServiceException(serviceException))) =>
      serviceException.getConnectionId match {
        case null         => this
        case connectionId => receivedConnectionId(headers.correlationId, connectionId)
      }
    case FromProvider(NsiRequesterMessage(headers, message: ReserveConfirmed)) =>
      receivedConnectionId(headers.correlationId, message.connectionId)
    case FromProvider(NsiRequesterMessage(headers, message: ReserveFailed)) =>
      receivedConnectionId(headers.correlationId, message.connectionId)
    case _ =>
      this
  }

  private def initialCorrelationIds: Set[CorrelationId] = segments.map(_._1)(collection.breakOut)
}
