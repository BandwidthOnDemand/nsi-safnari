package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType._
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateType

case class ProvisionStateMachineData(children: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, ProvisionStateEnumType], correlationId: Option[CorrelationId] = None) {

  def initialize(children: Map[ConnectionId, ProviderEndPoint]) =
    ProvisionStateMachineData(children, children.keys.map(_ -> RELEASED).toMap)

  def aggregatedProvisionStatus: ProvisionStateEnumType =
    if (childStates.values.exists(_ == UNKNOWN)) UNKNOWN
    else if (childStates.values.exists(_ == RELEASING)) RELEASING
    else if (childStates.values.forall(_ == RELEASED)) RELEASED
    else if (childStates.values.exists(_ == PROVISIONING)) PROVISIONING
    else if (childStates.values.forall(_ == PROVISIONED)) PROVISIONED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childStates.values}")

  def updateChild(connectionId: ConnectionId, state: ProvisionStateEnumType) =
    copy(childStates = childStates.updated(connectionId, state))

  def childHasState(connectionId: ConnectionId, state: ProvisionStateEnumType) =
    childStates.getOrElse(connectionId, UNKNOWN) == state
}

class ProvisionStateMachine(connectionId: ConnectionId, newCorrelationId: () => CorrelationId, outbound: Message => Unit)
  extends FiniteStateMachine[ProvisionStateEnumType, ProvisionStateMachineData](UNKNOWN, ProvisionStateMachineData(Map.empty, Map.empty)) {

  when(UNKNOWN) {
    case Event(children: Map[_, _], data) =>
      goto(RELEASED) using data.initialize(children.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
  }

  when(RELEASED) {
    case Event(FromRequester(message: Provision), data) =>
      goto(PROVISIONING) using data.copy(correlationId = Some(message.correlationId)) replying GenericAck(message.correlationId)
  }

  when(PROVISIONING) {
    case Event(FromProvider(message: ProvisionConfirmed), data) if data.childHasState(message.connectionId, RELEASED) =>
      val newData = data.updateChild(message.connectionId, PROVISIONED)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.correlationId)
  }

  when(PROVISIONED) {
    case Event(FromRequester(message: Release), data) =>
      goto(RELEASING) using data.copy(correlationId = Some(message.correlationId)) replying GenericAck(message.correlationId)

  }

  when(RELEASING) {
    case Event(FromProvider(message: ReleaseConfirmed), data) if data.childHasState(message.connectionId, PROVISIONED) =>
      val newData = data.updateChild(message.connectionId, RELEASED)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.correlationId)
  }

  onTransition {
    case RELEASED -> PROVISIONING =>
      stateData.children.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Provision(newCorrelationId(), connectionId), provider))
      }
    case PROVISIONED -> RELEASING =>
      stateData.children.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Release(newCorrelationId(), connectionId), provider))
      }
    case RELEASING -> RELEASED =>
      outbound(ToRequester(ReleaseConfirmed(stateData.correlationId.get, connectionId)))
    case PROVISIONING -> PROVISIONED =>
      outbound(ToRequester(ProvisionConfirmed(stateData.correlationId.get, connectionId)))
  }

  def provisionState = new ProvisionStateType().withState(stateName)

  def childState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
