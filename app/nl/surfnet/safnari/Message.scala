/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
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

import java.net.URI
import nl.surfnet.nsiv2.messages._
import org.joda.time.DateTime

sealed trait Message {
  def toShortString: String
  def correlationId: CorrelationId
}
object Message {
  private[safnari] def shortString(messageType: Class[_], operationType: Class[_], correlationId: CorrelationId) =
    s"${messageType.getSimpleName()}(cid=$correlationId, ${operationType.getSimpleName()})"
}

sealed trait OutboundMessage extends Message
sealed trait InboundMessage extends Message

final case class FromRequester(message: NsiProviderMessage[NsiProviderOperation]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class ToRequester(message: NsiRequesterMessage[NsiRequesterOperation]) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}

final case class ToProvider(message: NsiProviderMessage[NsiProviderOperation], provider: ProviderEndPoint) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class AckFromProvider(message: NsiProviderMessage[NsiAcknowledgement]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}
final case class FromProvider(message: NsiRequesterMessage[NsiRequesterOperation]) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.body.getClass(), correlationId)
  override def correlationId = message.headers.correlationId
}

final case class FromPce(message: PceResponse) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
final case class AckFromPce(message: PceAcknowledgement) extends InboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}
final case class ToPce(message: PceRequest) extends OutboundMessage {
  override def toShortString = Message.shortString(getClass(), message.getClass(), correlationId)
  override def correlationId = message.correlationId
}

final case class MessageDeliveryFailure(override val correlationId: CorrelationId, connectionId: Option[ConnectionId], originalCorrelationId: CorrelationId, uri: URI, timestamp: DateTime, message: String) extends InboundMessage {
  override def toShortString = s"${getClass().getSimpleName()}(correlationId=$correlationId, connectionId=$connectionId, originalCorrelationId=$originalCorrelationId, uri=$uri, timestamp=$timestamp, message=$message)"
}
final case class PassedEndTime(override val correlationId: CorrelationId, connectionId: ConnectionId, timestamp: DateTime) extends InboundMessage {
  override def toShortString = s"${getClass().getSimpleName()}(correlationId=$correlationId, connectionId=$connectionId, timestamp=$timestamp)"
}
