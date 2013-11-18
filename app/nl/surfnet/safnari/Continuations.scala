package nl.surfnet.safnari

import akka.actor.Scheduler
import java.util.concurrent.CancellationException
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.stm.TMap
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import akka.actor.Cancellable
import java.util.concurrent.TimeoutException

/**
 * Track all messages that await an asynchronous reply. Thread safe.
 *
 * @param scheduler The scheduler used for timeout management. Passed by-name to ensure the
 *   currently active scheduler is used, even from unit tests.
 */
class Continuations[T](scheduler: => Scheduler) {
  private val continuations = TMap.empty[CorrelationId, (Cancellable, Promise[T])].single

  /**
   * Register a continuation. When the continuation is completed the future will be successful.
   * If the continuation is not completed within the specified timeout the returned future will
   * fail with a [[java.concurrent.TimeoutException]].
   */
  def register(correlationId: CorrelationId, within: FiniteDuration): Future[T] = {
    val timeout = scheduler.scheduleOnce(within) {
      continuations.remove(correlationId).foreach {
        case (_, promise) =>
          promise.tryFailure(new TimeoutException(s"continuation not completed within $within"))
      }
    }

    val promise = Promise[T]
    continuations(correlationId) = (timeout, promise)
    promise.future
  }

  def unregister(correlationId: CorrelationId): Boolean = continuations.remove(correlationId) match {
    case None =>
      false
    case Some((timeout, _)) =>
      timeout.cancel()
      true
  }

  def replyReceived(correlationId: CorrelationId, reply: T): Unit =
    continuations.remove(correlationId).foreach {
      case (timeout, promise) =>
        timeout.cancel()
        promise.trySuccess(reply)
    }
}
