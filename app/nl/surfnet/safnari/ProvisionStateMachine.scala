package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateEnumType._
import org.ogf.schemas.nsi._2013._04.connection.types.ProvisionStateType

case class ProvisionStateMachineData(children: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, ProvisionStateEnumType], commandHeaders: Option[NsiHeaders] = None) {

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

class ProvisionStateMachine(connectionId: ConnectionId, newNsiHeaders: ProviderEndPoint => NsiHeaders, outbound: Message => Unit)
  extends FiniteStateMachine[ProvisionStateEnumType, ProvisionStateMachineData](UNKNOWN, ProvisionStateMachineData(Map.empty, Map.empty)) {

  when(UNKNOWN) {
    case Event(children: Map[_, _], data) =>
      goto(RELEASED) using data.initialize(children.map(p => p._1.asInstanceOf[ConnectionId] -> p._2.asInstanceOf[ProviderEndPoint]))
  }

  when(RELEASED) {
    case Event(FromRequester(message: Provision), data) =>
      goto(PROVISIONING) using data.copy(commandHeaders = Some(message.headers)) replying GenericAck(message.headers.asReply)
  }

  when(PROVISIONING) {
    case Event(FromProvider(message: ProvisionConfirmed), data) if data.childHasState(message.connectionId, RELEASED) =>
      val newData = data.updateChild(message.connectionId, PROVISIONED)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.headers.asReply)
  }

  when(PROVISIONED) {
    case Event(FromRequester(message: Release), data) =>
      goto(RELEASING) using data.copy(commandHeaders = Some(message.headers)) replying GenericAck(message.headers.asReply)

  }

  when(RELEASING) {
    case Event(FromProvider(message: ReleaseConfirmed), data) if data.childHasState(message.connectionId, PROVISIONED) =>
      val newData = data.updateChild(message.connectionId, RELEASED)
      goto(newData.aggregatedProvisionStatus) using newData replying GenericAck(message.headers.asReply)
  }

  onTransition {
    case RELEASED -> PROVISIONING =>
      stateData.children.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Provision(newNsiHeaders(provider), connectionId), provider))
      }
    case PROVISIONED -> RELEASING =>
      stateData.children.foreach {
        case (connectionId, provider) =>
          outbound(ToProvider(Release(newNsiHeaders(provider), connectionId), provider))
      }
    case RELEASING -> RELEASED =>
      outbound(ToRequester(ReleaseConfirmed(stateData.commandHeaders.get.asReply, connectionId)))
    case PROVISIONING -> PROVISIONED =>
      outbound(ToRequester(ProvisionConfirmed(stateData.commandHeaders.get.asReply, connectionId)))
  }

  def provisionState = new ProvisionStateType().withState(stateName)

  def childState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
