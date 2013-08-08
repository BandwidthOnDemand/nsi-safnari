package controllers

import play.api.test._
import play.api.test.Helpers._
import nl.surfnet.safnari._
import org.specs2.execute.PendingUntilFixed
import play.api.libs.concurrent.Akka
import akka.testkit.TestActorRef
import akka.pattern.ask
import akka.actor.Actor
import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionManagerSpec extends helpers.Specification {
  import NsiMessageSpec._

  class RecordingActor extends Actor {
    var messages = Vector.empty[Any]

    def receive = {
      case m => messages :+= m
    }
  }

  class Fixture() extends WithApplication(FakeApplication(additionalConfiguration = testConfiguration)) {
    implicit lazy val system = Akka.system

    lazy val outbound = TestActorRef(new RecordingActor)

    lazy val connectionManager = new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, "agg-nsa-id", URI.create("http://localhost"), URI.create("http://localhost")) {
      override def process(message: InboundMessage) = Some(Seq.empty)
    }))
  }

  "Connection manager" should {
    "find a created connection" in new Fixture() {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)
      val data = await(actor ? 'query).asInstanceOf[QuerySummaryResultType]

      connectionManager.get(data.getConnectionId) must beSome(actor)
    }

    "add child connection id" in new Fixture() {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)

      await(actor ? FromProvider(reserveConfirmed))

      connectionManager.findByChildConnectionId(reserveConfirmed.connectionId) must beSome(actor)
    }

    "add child connection id during replay" in new Fixture() {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)

      await(actor ? connectionManager.Replay(Seq(FromProvider(reserveConfirmed))))

      connectionManager.findByChildConnectionId(reserveConfirmed.connectionId) must beSome(actor)
    }
  }
}
