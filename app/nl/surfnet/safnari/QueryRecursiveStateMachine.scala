package nl.surfnet.safnari

import scala.collection.JavaConverters._
import org.ogf.schemas.nsi._2013._07.connection.types._

object QueryRecursiveState extends Enumeration {
  type QueryRecursiveState = Value
  val Initial, Collecting, Collected, Failed = Value
}

import QueryRecursiveState._

case class QueryRecursiveStateMachineData(
    childStates: Map[ConnectionId, QueryRecursiveState],
    answers: Map[ConnectionId, NsiRequesterOperation] = Map.empty,
    segments: Map[CorrelationId, ConnectionId] = Map.empty) {

  def aggregatedState: QueryRecursiveState =
    if (childStates.values.exists(_ == Collecting)) Collecting
    else if (childStates.values.forall(s => s == Failed || s == Collected))
      if (childStates.values.exists(_ == Failed)) Failed else Collected
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childStates.values}")

  def start: QueryRecursiveStateMachineData =
    this.copy(childStates = childStates.map(_._1 -> Collecting), segments = childStates.map(newCorrelationId -> _._1))

  def updateChild(correlationId: CorrelationId, state: QueryRecursiveState, answer: NsiRequesterOperation): QueryRecursiveStateMachineData =
    this.copy(childStates.updated(segments(correlationId), state), answers.updated(segments(correlationId), answer))
}

class QueryRecursiveStateMachine(id: ConnectionId, query: NsiProviderMessage[QueryRecursive], initialReserve: NsiProviderMessage[InitialReserve], connectionStates: => ConnectionStatesType, children: Map[ConnectionId, ProviderEndPoint], newNsiHeaders: ProviderEndPoint => NsiHeaders)
  extends FiniteStateMachine[QueryRecursiveState, QueryRecursiveStateMachineData, InboundMessage, OutboundMessage](
    Initial,
    QueryRecursiveStateMachineData(children.map(_._1 -> Initial))) {

  when(Initial) {
    case Event(FromRequester(NsiProviderMessage(_, _: QueryRecursive)), data) =>
      goto(Collecting) using data.start
  }

  when(Collecting) {
    case Event(FromProvider(NsiRequesterMessage(headers, qc: QueryRecursiveConfirmed)), data) =>
      val newData = data.updateChild(headers.correlationId, Collected, qc)
      goto(newData.aggregatedState) using newData
    case Event(FromProvider(NsiRequesterMessage(headers, qf: QueryRecursiveFailed)), data) =>
      val newData = data.updateChild(headers.correlationId, Failed, qf)
      goto(newData.aggregatedState) using newData
  }

  when(Collected)(PartialFunction.empty)
  when(Failed)(PartialFunction.empty)

  onTransition {
    case Initial -> Collecting =>
      nextStateData.segments.map {
        case (correlationId, connectionId) =>
          ToProvider(NsiProviderMessage(
            newNsiHeaders(children(connectionId)).copy(correlationId = correlationId),
            QueryRecursive(Some(Left(connectionId :: Nil)))),
            children(connectionId))
      }.toSeq

    case Collecting -> Collected =>
      val childRecursiveTypes = nextStateData.answers.collect {
        case (connectionId, QueryRecursiveConfirmed(Seq(result))) =>
          new ChildRecursiveType()
            .withConnectionId(result.getConnectionId())
            .withConnectionStates(result.getConnectionStates())
            .withProviderNSA(children(connectionId).nsa)
            .withCriteria(result.getCriteria())
      }

      val resultType = new QueryRecursiveResultType()
        .withRequesterNSA(initialReserve.headers.requesterNSA)
        .withConnectionId(id)
        .withConnectionStates(connectionStates)
        .withCriteria(new QueryRecursiveResultCriteriaType()
          .withSchedule(initialReserve.body.criteria.getSchedule())
          .withServiceType(initialReserve.body.criteria.getServiceType())
          .withVersion(initialReserve.body.criteria.getVersion())
          .withChildren(new ChildRecursiveListType()
            .withChild(childRecursiveTypes.toList.asJava)))

      Seq(ToRequester(query reply QueryRecursiveConfirmed(resultType :: Nil)))
    case Collecting -> Failed =>
      // TODO
      val body = QueryRecursiveFailed(new QueryFailedType())
      Seq(ToRequester(query reply body))
  }
}
