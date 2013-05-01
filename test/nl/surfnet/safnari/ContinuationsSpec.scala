package nl.surfnet.safnari

import play.api.test.Helpers._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ContinuationsSpec extends org.specs2.mutable.Specification {
  trait fixture extends org.specs2.specification.Scope {
    val CorrelationId = newCorrelationId
    val continuations = new Continuations[String]()
  }

  "Continuations" should {
    "invoke callback when reply is received" in new fixture {
      val reply = continuations.register(CorrelationId)

      continuations.replyReceived(CorrelationId, "reply")

      await(reply) must beEqualTo("reply")
    }

    "ignore replies with unregistered correlation ids" in new fixture {
      continuations.replyReceived(CorrelationId, "reply") must not(throwA[Exception])
    }

    "remove callback after first reply is received" in new fixture {
      val reply = continuations.register(CorrelationId)
      continuations.replyReceived(CorrelationId, "first-reply")

      continuations.replyReceived(CorrelationId, "second-reply")

      await(reply) must beEqualTo("first-reply")
    }
  }
}
