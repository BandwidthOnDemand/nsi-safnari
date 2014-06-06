package nl.surfnet.safnari

case class ChildConnectionIds(
  childrenByCorrelationId: Map[CorrelationId, ProviderEndPoint] = Map.empty,
  connectionByInitialCorrelationId: Map[CorrelationId, ConnectionId] = Map.empty,
  initialCorrelationIdByConnectionId: Map[ConnectionId, CorrelationId] = Map.empty) {

  def withChildren(childrenByCorrelationId: Map[CorrelationId, ProviderEndPoint]) =
    copy(childrenByCorrelationId = childrenByCorrelationId)

  def childrenByConnectionId: Map[ConnectionId, ProviderEndPoint] = for {
    (correlationId, provider) <- childrenByCorrelationId
    connectionId <- connectionByInitialCorrelationId.get(correlationId)
  } yield connectionId -> provider

  def awaitingConnectionId: Set[CorrelationId] = childrenByCorrelationId.keySet -- connectionByInitialCorrelationId.keySet

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId) = {
    copy(
      connectionByInitialCorrelationId = connectionByInitialCorrelationId.updated(correlationId, connectionId),
      initialCorrelationIdByConnectionId = initialCorrelationIdByConnectionId.updated(connectionId, correlationId))
  }
}
