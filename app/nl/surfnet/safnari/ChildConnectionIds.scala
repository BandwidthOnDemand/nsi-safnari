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
import nl.surfnet.nsiv2.messages.{ReserveFailed, ReserveConfirmed}

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

  def receivedConnectionId(correlationId: CorrelationId, connectionId: ConnectionId) = if (segments.exists(_._1 == correlationId)) {
    copy(
      connectionByInitialCorrelationId = connectionByInitialCorrelationId.updated(correlationId, connectionId),
      initialCorrelationIdByConnectionId = initialCorrelationIdByConnectionId.updated(connectionId, correlationId))
  } else this

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
