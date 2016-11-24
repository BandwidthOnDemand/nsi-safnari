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

import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._

import org.ogf.schemas.nsi._2013._12.connection.types.ProvisionStateEnumType
import org.ogf.schemas.nsi._2013._12.connection.types.ProvisionStateEnumType._

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

class ProvisionStateMachine(
  connectionId: ConnectionId,
  newRequestHeaders: (NsiProviderMessage[NsiProviderOperation], ProviderEndPoint) => NsiHeaders,
  children: Map[ConnectionId, ProviderEndPoint]
) extends FiniteStateMachine[ProvisionStateEnumType, ProvisionStateMachineData, InboundMessage, OutboundMessage](
  RELEASED,
  ProvisionStateMachineData(children, children.map(_._1 -> RELEASED))
) {

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

  whenUnhandled {
    case Event(AckFromProvider(_), _) => stay
  }

  onTransition {
    case RELEASED -> PROVISIONING =>
      stateData.children.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newRequestHeaders(nextStateData.command.get, provider), Provision(connectionId)), provider)
      }.toVector
    case PROVISIONED -> RELEASING =>
      stateData.children.map {
        case (connectionId, provider) =>
          ToProvider(NsiProviderMessage(newRequestHeaders(nextStateData.command.get, provider), Release(connectionId)), provider)
      }.toVector
    case RELEASING -> RELEASED =>
      Seq(ToRequester(stateData.command.get reply ReleaseConfirmed(connectionId)))
    case PROVISIONING -> PROVISIONED =>
      Seq(ToRequester(stateData.command.get reply ProvisionConfirmed(connectionId)))
  }

  def provisionState = stateName

  def childConnectionState(connectionId: ConnectionId) = stateData.childStates(connectionId)
}
