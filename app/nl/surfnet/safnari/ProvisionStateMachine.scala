package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType._
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateType

case class ProvisionStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], states: Map[ConnectionId, ProvisionStateEnumType]) {
  def initialize(providers: Map[ConnectionId, ProviderEndPoint]): ProvisionStateMachineData = {
    ProvisionStateMachineData(providers, providers.keys.map(_ -> UNKNOWN).toMap)
  }

  def aggregatedProvisionStatus: ProvisionStateEnumType = {
    if (states.values.exists(_ == UNKNOWN)) UNKNOWN
    else if (states.values.exists(_ == RELEASING)) RELEASING
    else if (states.values.exists(_ == PROVISIONING)) PROVISIONING
    else if (states.values.forall(_ == RELEASED)) RELEASED
    else if (states.values.forall(_ == PROVISIONED)) PROVISIONED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${states.values}")
  }

  def updateState(connectionId: ConnectionId, state: ProvisionStateEnumType): ProvisionStateMachineData = {
    copy(states = states + (connectionId -> state))
  }
}

class ProvisionStateMachine(newCorrelationId: () => CorrelationId, outbound: Message => Unit)
  extends FiniteStateMachine[ProvisionStateEnumType, ProvisionStateMachineData](UNKNOWN, ProvisionStateMachineData(Map.empty, Map.empty)) {

  when(UNKNOWN) {
    case Event(downstreamConnections: Map[_, _], data) =>
      goto(RELEASED) using data.initialize(downstreamConnections.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
  }

  when(RELEASED) {
    case Event(FromRequester(message: Provision), _) =>
      goto(PROVISIONING) replying GenericAck(message.correlationId)
  }

  when(PROVISIONING) {
    case Event(FromProvider(message: ProvisionConfirmed), data) =>
      val newData = data.updateState(message.connectionId, PROVISIONED)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.correlationId)
  }

  when(PROVISIONED) {
    case Event(FromRequester(message: Release), _) =>
      goto(RELEASING) replying GenericAck(message.correlationId)

  }

  when(RELEASING) {
    case Event(FromProvider(message: ReleaseConfirmed), data) =>
      val newData = data.updateState(message.connectionId, RELEASED)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.correlationId)
  }

  onTransition {
    case RELEASED -> PROVISIONING =>
      stateData.providers.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Provision(newCorrelationId(), connectionId), provider))
      }
    case PROVISIONED -> RELEASING =>
      stateData.providers.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Release(newCorrelationId(), connectionId), provider))
      }
  }

  def provisionState(reservationVersion: Int) = stateName match {
    case UNKNOWN => new ProvisionStateType().withState(UNKNOWN).withVersion(null)
    case state   => new ProvisionStateType().withState(state).withVersion(reservationVersion)
  }
}
