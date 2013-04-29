package nl.surfnet.nsi

import scala.concurrent.stm.TMap
import scala.concurrent.Future
import scala.concurrent.Promise

/**
 * Track all messages that await an asynchronous reply. Thread safe.
 */
class Continuations[T] {
  private val continuations = TMap.empty[CorrelationId, Promise[T]].single

  def register(correlationId: CorrelationId): Future[T] = {
    val promise = Promise[T]
    continuations(correlationId) = promise
    promise.future
  }

  def replyReceived(correlationId: CorrelationId, reply: T): Unit =
    continuations.remove(correlationId).foreach { _.trySuccess(reply) }
}
