package nl.surfnet.safnari

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._

import scala.Error
import scala.collection.JavaConverters._
import org.ogf.schemas.nsi._2013._12.connection.types._

object QueryRecursiveState extends Enumeration {
  type QueryRecursiveState = Value
  val Initial, Collecting, Collected, Failed = Value
}

import QueryRecursiveState._

case class QueryRecursiveStateMachineData(
    providers: Map[ConnectionId, ProviderEndPoint],
    childStates: Map[ConnectionId, QueryRecursiveState],
    answers: Map[ConnectionId, NsiRequesterOperation] = Map.empty,
    segments: Map[CorrelationId, ConnectionId] = Map.empty) {

  def aggregatedState: QueryRecursiveState =
    if (childStates.isEmpty) Collected
    else if (childStates.values.exists(_ == Collecting)) Collecting
    else if (childStates.values.forall(s => s == Failed || s == Collected))
      if (childStates.values.exists(_ == Failed)) Failed else Collected
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childStates.values}")

  def start(segs: Map[CorrelationId, ConnectionId]): QueryRecursiveStateMachineData =
    this.copy(childStates = childStates.map(_._1 -> Collecting), segments = segs)

  def updateChild(correlationId: CorrelationId, state: QueryRecursiveState, answer: NsiRequesterOperation): QueryRecursiveStateMachineData =
    this.copy(
      childStates = childStates.updated(segments(correlationId), state),
      answers = answers.updated(segments(correlationId), answer))
}

class QueryRecursiveStateMachine(
    id: ConnectionId,
    query: NsiProviderMessage[QueryRecursive],
    initialReserve: NsiProviderMessage[InitialReserve],
    connectionStates: => ConnectionStatesType,
    children: Map[ProviderEndPoint, Option[ConnectionId]],
    newCorrelationId: () => CorrelationId,
    newNsiHeaders: ProviderEndPoint => NsiHeaders)
  extends FiniteStateMachine[QueryRecursiveState, QueryRecursiveStateMachineData, InboundMessage, OutboundMessage](
    Initial,
    QueryRecursiveStateMachineData(
      children.collect(QueryRecursiveStateMachine.toConnectionIdProviderMap),
      children.collect(QueryRecursiveStateMachine.toConnectionIdStateMap))) {

  when(Initial) {
    case Event(FromRequester(NsiProviderMessage(_, _: QueryRecursive)), data) =>
      val segments = data.providers.map(newCorrelationId() -> _._1)
      val newData = data.start(segments)
      goto(newData.aggregatedState) using newData
  }

  when(Collecting) {
    case Event(FromProvider(NsiRequesterMessage(headers, queryResult: QueryRecursiveConfirmed)), data) =>
      val newData = data.updateChild(headers.correlationId, Collected, queryResult)
      goto(newData.aggregatedState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, error @ ErrorReply(_))), data) =>
      val newData = data.updateChild(headers.correlationId, Failed, error)
      goto(newData.aggregatedState) using newData
  }

  when(Collected)(PartialFunction.empty)
  when(Failed)(PartialFunction.empty)

  onTransition {
    case Initial -> Collecting =>
      nextStateData.segments.map {
        case (correlationId, connectionId) =>
          val provider = nextStateData.providers(connectionId)
          ToProvider(NsiProviderMessage(
            newNsiHeaders(provider).copy(correlationId = correlationId),
            QueryRecursive(Some(Left(connectionId :: Nil)))),
            provider)
      }.toSeq

    case Initial -> Collected =>
      Seq(ToRequester(query reply QueryRecursiveConfirmed(queryRecursiveResultType(Nil) :: Nil)))
    case Collecting -> Collected =>
      val childRecursiveTypes = nextStateData.answers.collect {
        case (connectionId, QueryRecursiveConfirmed(Seq(result))) =>
          new ChildRecursiveType()
            .withConnectionId(result.getConnectionId())
            .withConnectionStates(result.getConnectionStates())
            .withProviderNSA(nextStateData.providers(connectionId).nsa)
            .withCriteria(result.getCriteria())
      }.toList

      Seq(ToRequester(query reply QueryRecursiveConfirmed(queryRecursiveResultType(childRecursiveTypes) :: Nil)))
    case Collecting -> Failed =>
      val queryFailed = nextStateData.answers.collectFirst {
        case (connectionId, failed @ ErrorReply(_)) => failed
      }

      Seq(ToRequester(query reply queryFailed.get))
  }

  private def queryRecursiveResultType(childs: List[ChildRecursiveType]): QueryRecursiveResultType = {
    new QueryRecursiveResultType()
      .withRequesterNSA(initialReserve.headers.requesterNSA)
      .withConnectionId(id)
      .withConnectionStates(connectionStates)
      .withCriteria(new QueryRecursiveResultCriteriaType()
        .withSchedule(initialReserve.body.criteria.getSchedule())
        .withServiceType(initialReserve.body.criteria.getServiceType())
        .withVersion(initialReserve.body.criteria.getVersion())
        .withChildren(new ChildRecursiveListType().withChild(childs.asJava)))
  }

}

object QueryRecursiveStateMachine {
  val toConnectionIdProviderMap: PartialFunction[(ProviderEndPoint, Option[ConnectionId]), (ConnectionId, ProviderEndPoint)] = {
    case (provider, Some(connectionId)) => connectionId -> provider
  }

  val toConnectionIdStateMap: PartialFunction[(ProviderEndPoint, Option[ConnectionId]), (ConnectionId, QueryRecursiveState)] = {
    case (provider, Some(connectionId)) => connectionId -> Initial
  }
}
