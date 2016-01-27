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

import nl.surfnet.nsiv2.utils._

import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.nsiv2.messages.{ DataPlaneStateChange, NsiRequesterMessage, NsiHeaders }
import org.ogf.schemas.nsi._2013._12.connection.types.DataPlaneStateChangeRequestType
import org.ogf.schemas.nsi._2013._12.connection.types.DataPlaneStatusType

case class DataPlaneStateMachineData(providers: Map[ConnectionId, ProviderEndPoint], childStates: Map[ConnectionId, DataPlaneStatusType], timeStamp: Option[XMLGregorianCalendar]) {

  def highestVersion: Int = childStates.values.map(_.getVersion).max

  def isActive: Boolean = childStates.values.forall(_.isActive)

  def isVersionConsistent: Boolean = childStates.values.forall(_.isVersionConsistent) && childStates.values.forall(_.getVersion == highestVersion)

  def dataPlaneStatus = new DataPlaneStatusType()
    .withVersion(highestVersion)
    .withActive(isActive)
    .withVersionConsistent(isVersionConsistent)

  def updateState(connectionId: ConnectionId, state: DataPlaneStatusType, newTimeStamp: XMLGregorianCalendar) = {
    val latestTimeStamp = timeStamp.map(implicitly[Ordering[XMLGregorianCalendar]].max(_, newTimeStamp)).orElse(Some(newTimeStamp))
    copy(childStates = childStates + (connectionId -> state), timeStamp = latestTimeStamp)
  }
}

class DataPlaneStateMachine(connectionId: ConnectionId, newNotificationHeaders: () => NsiHeaders, newNotificationId: () => Int, children: Map[ConnectionId, ProviderEndPoint])
  extends FiniteStateMachine[Unit, DataPlaneStateMachineData, InboundMessage, OutboundMessage](
    (),
    DataPlaneStateMachineData(
      children,
      children.map(_._1 -> new DataPlaneStatusType()),
      None)) {

  when(()) {
    case Event(FromProvider(NsiRequesterMessage(headers, DataPlaneStateChange(notification))), data) =>
      val newData = data.updateState(notification.getConnectionId, notification.getDataPlaneStatus(), notification.getTimeStamp())
      stay using newData
  }

  onTransition {
    case _ =>
      val previous = stateData.dataPlaneStatus
      val next = nextStateData.dataPlaneStatus
      if (previous == next)
        Seq.empty
      else
        Seq(ToRequester(NsiRequesterMessage(newNotificationHeaders(), DataPlaneStateChange(new DataPlaneStateChangeRequestType()
          .withConnectionId(connectionId)
          .withNotificationId(newNotificationId())
          .withDataPlaneStatus(next)
          .withTimeStamp(nextStateData.timeStamp.get)))))
  }

  def dataPlaneStatus = nextStateData.dataPlaneStatus

  def childConnectionState(connectionId: ConnectionId) = nextStateData.childStates(connectionId)
}
