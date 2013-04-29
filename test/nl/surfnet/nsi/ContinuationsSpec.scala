package nl.surfnet.nsi

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ContinuationsSpec extends org.specs2.mutable.Specification {
  trait fixture extends org.specs2.specification.Scope {
    val CorrelationId = newCorrelationId
    val continuations = new Continuations[String]()
    var reply: String = null
  }

  "Continuations" should {
    "invoke callback when reply is received" in new fixture {
      continuations.register(CorrelationId)(reply = _)

      continuations.replyReceived(CorrelationId, "reply")

      reply must beEqualTo("reply")
    }

    "ignore replies with unregistered correlation ids" in new fixture {
      continuations.replyReceived(CorrelationId, "reply")

      reply must beNull
    }

    "remove callback after first reply is received" in new fixture {
      continuations.register(CorrelationId)(reply = _)
      continuations.replyReceived(CorrelationId, "first-reply")

      continuations.replyReceived(CorrelationId, "second-reply")

      reply must beEqualTo("first-reply")
    }
  }
}
