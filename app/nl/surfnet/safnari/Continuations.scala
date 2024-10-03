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

import akka.actor.Scheduler
import nl.surfnet.nsiv2.messages.CorrelationId
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.*
import scala.concurrent.stm.TMap
import akka.actor.Cancellable
import java.util.concurrent.TimeoutException

/** Track all messages that await an asynchronous reply. Thread safe.
  *
  * @param scheduler
  *   The scheduler used for timeout management. Passed by-name to ensure the currently active
  *   scheduler is used, even from unit tests.
  */
class Continuations[T](scheduler: => Scheduler)(implicit ec: ExecutionContext) {
  private val continuations = TMap.empty[CorrelationId, (Seq[Cancellable], Promise[T])].single

  /** Register a continuation. When the continuation is completed the future will be successful. If
    * the continuation is not completed within the specified timeout the returned future will fail
    * with a [[java.concurrent.TimeoutException]].
    */
  def register(correlationId: CorrelationId, within: FiniteDuration): Future[T] = {
    val timeout = scheduler.scheduleOnce(within) {
      continuations.remove(correlationId).foreach { case (_, promise) =>
        promise.tryFailure(new TimeoutException(s"continuation not completed within $within"))
      }
    }

    val promise = Promise[T]()
    continuations(correlationId) = (timeout :: Nil, promise)
    promise.future
  }

  def addTimeout(correlationId: CorrelationId, within: FiniteDuration)(
      timeoutCallback: => Unit
  ): Unit = {
    continuations.get(correlationId).foreach { case (timeouts, promise) =>
      val timeout = scheduler.scheduleOnce(within)(timeoutCallback)
      continuations.update(correlationId, ((timeout +: timeouts), promise))
    }
  }

  def unregister(correlationId: CorrelationId): Boolean =
    continuations.remove(correlationId) match {
      case None =>
        false
      case Some((timeouts, _)) =>
        timeouts.foreach(_.cancel())
        true
    }

  def replyReceived(correlationId: CorrelationId, reply: T): Unit =
    continuations.remove(correlationId).foreach { case (timeouts, promise) =>
      timeouts.foreach(_.cancel())
      promise.trySuccess(reply)
    }
}
