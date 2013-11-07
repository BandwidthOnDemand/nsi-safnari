package nl.surfnet.safnari

import akka.actor.ActorSystem
import java.util.concurrent.TimeoutException
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Failure

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ContinuationsSpec extends helpers.Specification {
  play.api.Logger("initialize-loggers-to-avoid-warnings")

  trait fixture extends org.specs2.mutable.After {
    val actorSystem = ActorSystem("test")

    val CorrelationId = newCorrelationId
    val continuations = new Continuations[String](actorSystem.scheduler)

    override def after = {
      actorSystem.shutdown
      actorSystem.awaitTermination
    }
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

      Await.ready(reply, 100.milliseconds).value must beSome.like {
        case Failure(e) => e must beAnInstanceOf[TimeoutException]
      }

      continuations.unregister(CorrelationId) aka "registered" must beFalse
    }
  }
}
