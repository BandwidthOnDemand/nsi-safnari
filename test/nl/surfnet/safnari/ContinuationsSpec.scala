package nl.surfnet.safnari

import akka.actor.ActorSystem
import java.util.concurrent.TimeoutException
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*
import scala.util.Failure
import scala.util.Try
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import akka.actor.Terminated
import nl.surfnet.nsiv2.messages

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ContinuationsSpec extends helpers.Specification {
  sequential

  trait fixture extends org.specs2.mutable.After {
    val actorSystem: ActorSystem = ActorSystem("test")

    val CorrelationId: messages.CorrelationId = newCorrelationId()
    val continuations = new Continuations[String](actorSystem.scheduler)(ExecutionContext.global)

    override def after: Terminated = Await.result(actorSystem.terminate(), 10.seconds)
  }

  "Continuations" should {

    "complete future successfully when reply is received" in new fixture {
      val reply = continuations.register(CorrelationId, within = 120.seconds)

      continuations.replyReceived(CorrelationId, "reply")

      await(reply) must beEqualTo("reply")
    }

    "ignore replies with unregistered correlation ids" in new fixture {
      continuations.replyReceived(CorrelationId, "reply") must not(throwA[Exception])
    }

    "remove callback after first reply is received" in new fixture {
      val reply = continuations.register(CorrelationId, within = 120.seconds)

      continuations.replyReceived(CorrelationId, "first-reply")

      continuations.unregister(CorrelationId) aka "registered" must beFalse
    }

    "not complete unregistered futures" in new fixture {
      val reply = continuations.register(CorrelationId, within = 50.milliseconds)
      continuations.unregister(CorrelationId)
      continuations.replyReceived(CorrelationId, "ignored-reply")

      Thread.sleep(100)

      reply.isCompleted aka "completed" must beFalse
    }

    "remove callback after timeout has been exceeded" in new fixture {
      val reply = continuations.register(CorrelationId, within = 10.milliseconds)

      Await.ready(reply, 100.milliseconds).value must beSome[Try[String]].like { case Failure(e) =>
        e must beAnInstanceOf[TimeoutException]
      }

      continuations.unregister(CorrelationId) aka "registered" must beFalse
    }

    "add timeout callback should not complete future" in new fixture {
      val countDown = new CountDownLatch(1)
      val reply = continuations.register(CorrelationId, 1.seconds)
      continuations.addTimeout(CorrelationId, 10.milliseconds) {
        countDown.countDown()
      }

      countDown.await(100, TimeUnit.MILLISECONDS) must beTrue
      reply.isCompleted must beFalse
    }
  }
}
