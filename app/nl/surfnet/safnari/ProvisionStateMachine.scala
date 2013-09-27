package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.ProvisionStateEnumType._

case class ProvisionStateMachineData(children: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, ProvisionStateEnumType], command: Option[NsiProviderMessage[NsiProviderOperation]] = None) {

  def aggregatedProvisionStatus: ProvisionStateEnumType =
    if (childStates.values.exists(_ == RELEASING)) RELEASING
    else if (childStates.values.forall(_ == RELEASED)) RELEASED
    else if (childStates.values.exists(_ == PROVISIONING)) PROVISIONING
    else if (childStates.values.forall(_ == PROVISIONED)) PROVISIONED
    else throw new IllegalStateException(s"cannot determine aggregated status from ${childStates.values}")

  def startCommand(newCommand: NsiProviderMessage[NsiProviderOperation], transitionalState: ProvisionStateEnumType) =
    copy(command = Some(newCommand), childStates = childStates.map(_._1 -> transitionalState))

  def updateChild(connectionId: ConnectionId, state: ProvisionStateEnumType) =
    copy(childStates = childStates.updated(connectionId, state))

  def childHasState(connectionId: ConnectionId, state: ProvisionStateEnumType) =
    childStates.getOrElse(connectionId, RELEASED) == state
}

class ProvisionStateMachine(connectionId: ConnectionId, newNsiHeaders: ProviderEndPoint => NsiHeaders, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[ProvisionStateEnumType, ProvisionStateMachineData, InboundMessage, OutboundMessage](RELEASED, ProvisionStateMachineData(children, children.map(_._1 -> RELEASED))) {

  when(RELEASED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Provision)), data) =>
      goto(PROVISIONING) using data.startCommand(message, PROVISIONING)
  }

  when(PROVISIONING) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ProvisionConfirmed)), data) if data.childHasState(message.connectionId, PROVISIONING) =>
      val newData = data.updateChild(message.connectionId, PROVISIONED)
      goto(newData.aggregatedProvisionStatus) using newData
  }

  when(PROVISIONED) {
    case Event(FromRequester(message @ NsiProviderMessage(_, _: Release)), data) =>
      goto(RELEASING) using data.startCommand(message, RELEASING)
  }

  when(RELEASING) {
    case Event(FromProvider(NsiRequesterMessage(headers, message: ReleaseConfirmed)), data) if data.childHasState(message.connectionId, RELEASING) =>
      val newData = data.updateChild(message.connectionId, RELEASED)
      goto(newData.aggregatedProvisionStatus) using newData
  }

  onTransition {
    case RELEASED -> PROVISIONING =>
      stateData.children.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(provider), Provision(connectionId)), provider)
      }.toVector
    case PROVISIONED -> RELEASING =>
      stateData.children.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newNsiHeaders(provider), Release(connectionId)), provider)
      }.toVector
    case RELEASING -> RELEASED =>
      Seq(ToRequester(stateData.command.get reply ReleaseConfirmed(connectionId)))
    case PROVISIONING -> PROVISIONED =>
      Seq(ToRequester(stateData.command.get reply ProvisionConfirmed(connectionId)))
  }

  def provisionState = stateName

  def childConnectionState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
