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
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.types.DirectionalityType
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionManagerSpec extends helpers.Specification {
  sequential

  import NsiMessageSpec._

  class RecordingActor extends Actor {
    @volatile var messages = Vector.empty[Any]

    def receive = {
      case m =>
        messages :+= m
    }
  }

  class DummyConnectionFixture extends WithApplication() {
    implicit lazy val system = Akka.system

    lazy val outbound = TestActorRef(new RecordingActor)

    lazy val connectionManager = new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, "agg-nsa-id", URI.create("http://localhost"), URI.create("http://localhost")) {
      override def process(message: InboundMessage) = Some(Seq.empty)
    }))
  }

  "Connection manager" should {
    "find a created connection" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)
      val data = await(actor ? 'query).asInstanceOf[QuerySummaryResultType]

      connectionManager.get(data.getConnectionId) must beSome(actor)
    }

    "add child connection id" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)

      await(actor ? FromProvider(reserveConfirmed))

      connectionManager.findByChildConnectionId(reserveConfirmed.body.connectionId) must beSome(actor)
    }

    "add child connection id during replay" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)

      await(actor ? connectionManager.Replay(Seq(FromProvider(reserveConfirmed))))

      connectionManager.findByChildConnectionId(reserveConfirmed.body.connectionId) must beSome(actor)
    }
  }

  class SingleConnectionActorFixture extends WithApplication() {
    implicit lazy val system = Akka.system

    lazy val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    lazy val outbound = TestActorRef(new RecordingActor)
    lazy val connectionManager = new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, "agg-nsa-id", URI.create("http://localhost"), URI.create("http://localhost"))))
    lazy val Some(connection) = connectionManager.findOrCreateConnection(initialReserveMessage)

    def output[A](f: => A): Vector[Any] = {
      outbound.underlyingActor.messages = Vector.empty
      f
      outbound.underlyingActor.messages
    }

  }

  "Connection actor" should {
    "ensure resend message is exactly the same as the original" in new SingleConnectionActorFixture {
      val OriginalReserve = InitialReserveType.withDescription("original")
      val ModifiedReserve = InitialReserveType.withDescription("modified")

      val ack = await(connection ? FromRequester(NsiProviderMessage(initialReserveMessage.headers, InitialReserve(OriginalReserve, ConfirmCriteria, Service))))
      val error = await(connection ? FromRequester(NsiProviderMessage(initialReserveMessage.headers, InitialReserve(ModifiedReserve, ConfirmCriteria, Service))))

      error must beLike {
        case details: ServiceExceptionType =>
          details must_== NsiError.PayloadError.toServiceException("FIXME-NSA-ID").withText(s"request with existing correlation id ${initialReserveMessage.headers.correlationId} does not match the original request")
      }
    }

    "resend original async reply when original request is resend" in new SingleConnectionActorFixture {
      val ack = await(connection ? FromRequester(initialReserveMessage))
      await(connection ? pce.confirm(CorrelationId(0, 1), A))

      val originalOutbound = output {
        await(connection ? upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)))
      }

      val resendOutbound = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      resendOutbound must_== originalOutbound
    }

    "resend ToProvider messages when no async replay has been received" in new SingleConnectionActorFixture {
      pending

      val originalOutbound = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      val resendOutbound = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      resendOutbound.pp("resend") must_== originalOutbound.pp("original")
    }
  }
}
