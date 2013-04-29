package nl.surfnet.nsi

import scala.concurrent.stm.TMap

/**
 * Track all messages that await an asynchronous reply. Thread safe.
 */
class Continuations[T] {
  private val continuations = TMap.empty[CorrelationId, T => Unit].single

  def register(correlationId: CorrelationId)(f: T => Unit): Unit =
    continuations(correlationId) = f

  def replyReceived(correlationId: CorrelationId, reply: T): Unit =
    continuations.remove(correlationId).foreach { f => f(reply) }
}
