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
import org.joda.time.DateTime

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

    lazy val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    lazy val outbound = TestActorRef(new RecordingActor)

    lazy val connectionManager = new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, AggregatorNsa, URI.create("http://localhost"), URI.create("http://localhost"))))
  }

  "Connection manager" should {
    "find a created connection" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)
      val (_, data: QuerySummaryResultType) = await(actor ? 'query)

      connectionManager.get(data.getConnectionId) must beSome(actor)
    }

    "add child connection id" in new DummyConnectionFixture {
      val initialReserve = ura.request(CorrelationId(0, 0), initialReserveMessage.body)
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserve.message.asInstanceOf[NsiProviderMessage[NsiProviderCommand]])
      await(actor ? initialReserve)
      await(actor ? pce.confirm(CorrelationId(0, 1), A))
      await(actor ? upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnectionId", initialReserveMessage.body.criteria)))

      connectionManager.findByChildConnectionId("ChildConnectionId") must beSome(actor)
    }

    "add child connection id during replay" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)

      await(actor ? connectionManager.Replay(Seq(FromRequester(initialReserveMessage), pce.confirm(CorrelationId(0, 1), A), upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)))))

      connectionManager.findByChildConnectionId("ChildConnection") must beSome(actor)
    }

    "find connection by requester correlationId for idempotent command handling" in new DummyConnectionFixture {
      val Some(actor1) = connectionManager.findOrCreateConnection(initialReserveMessage)
      val Some(actor2) = connectionManager.findOrCreateConnection(initialReserveMessage)

      actor1 must_== actor2
    }
  }

  class SingleConnectionActorFixture extends WithApplication() {
    implicit lazy val system = Akka.system
    implicit lazy val executionContext = system.dispatcher

    def createConnectionManager = {
      val mockUuidGenerator = Uuid.mockUuidGenerator(1)
      def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())
      new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, AggregatorNsa, URI.create("http://localhost"), URI.create("http://localhost"))))
    }

    lazy val outbound = TestActorRef(new RecordingActor)
    lazy val connectionManager = createConnectionManager
    lazy val Some(connection) = connectionManager.findOrCreateConnection(initialReserveMessage)

    def query = await((connection ? 'query).mapTo[(ReservationConfirmCriteriaType, QuerySummaryResultType)])._2
    lazy val connectionId = query.getConnectionId

    def reserveWithEndTime(endTime: DateTime): Unit = {
      val reserve = initialReserveMessage.tap(_.body.body.getCriteria().getSchedule().withEndTime(endTime.toXmlGregorianCalendar))
      Seq(
        FromRequester(reserve),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)),
        ura.request(CorrelationId(1, 1), ReserveCommit(connectionId)),
        upa.response(CorrelationId(0, 4), ReserveCommitConfirmed("ChildConnection"))).foreach {
          message => await(connection ? message)
        }
    }

    def output[A](f: => A): Vector[Any] = {
      outbound.underlyingActor.messages = Vector.empty
      f
      outbound.underlyingActor.messages
    }

  }

  "Connection actor" should {
    "persist and restore from the database" in new SingleConnectionActorFixture {
      Seq(FromRequester(initialReserveMessage), pce.confirm(CorrelationId(0, 1), A), upa.response(CorrelationId(0, 1), ReserveConfirmed("ChildConnection", ConfirmCriteria))).foreach(message => await(connection ? message))
      val restoredConnectionManager = createConnectionManager
      await(restoredConnectionManager.restore)

      val restoredConnection = restoredConnectionManager.get(connectionId)

      restoredConnection aka "restored connection" must beSome
      await(restoredConnection.get ? 'query) must_== await(connection ? 'query)
    }

    "ensure retransmitted message is exactly the same as the original" in new SingleConnectionActorFixture {
      val OriginalReserve = InitialReserveType.withDescription("original")
      val ModifiedReserve = InitialReserveType.withDescription("modified")

      val ack = await(connection ? FromRequester(NsiProviderMessage(initialReserveMessage.headers, InitialReserve(OriginalReserve, ConfirmCriteria, Service))))
      val error = await(connection ? FromRequester(NsiProviderMessage(initialReserveMessage.headers, InitialReserve(ModifiedReserve, ConfirmCriteria, Service))))

      error must beLike {
        case ServiceException(details) =>
          details must_== NsiError.PayloadError.toServiceException(AggregatorNsa).withText(s"duplicate request with existing correlation id ${initialReserveMessage.headers.correlationId} does not match the original")
      }
    }

    "retransmit original async reply on duplicate request" in new SingleConnectionActorFixture {
      val ack = await(connection ? FromRequester(initialReserveMessage))
      await(connection ? pce.confirm(CorrelationId(0, 1), A))

      val original = output {
        await(connection ? upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)))
      }

      val retransmitted = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      retransmitted must_== original
    }

    "retransmit PCE message when no async reply has been received" in new SingleConnectionActorFixture {
      val original = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      val retransmitted = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      retransmitted aka "retransmitted" must_== original
    }

    "retransmit ToProvider messages without async reply" in new SingleConnectionActorFixture {
      await(connection ? FromRequester(initialReserveMessage))
      val original = output {
        await(connection ? pce.confirm(CorrelationId(0, 1), A, B))
      }
      val retransmitted = output {
        await(connection ? FromRequester(initialReserveMessage))
      }
      retransmitted aka "retransmitted" must_== original

      await(connection ? upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnectionA", initialReserveMessage.body.criteria)))

      val retransmitted2 = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      retransmitted2 aka "retransmitted 2" must_== (original.collect {
        case message @ ToProvider(NsiProviderMessage(_, _), provider) if provider == B.provider => message
      })
    }

    "restore state from replayed messages" in new SingleConnectionActorFixture {
      await(connection ? connectionManager.Replay(Seq(FromRequester(initialReserveMessage), pce.confirm(CorrelationId(0, 1), A), upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)))))

      val retransmitted = output {
        await(connection ? FromRequester(initialReserveMessage))
      }

      retransmitted must_== Vector(agg.response(initialReserveMessage.headers.correlationId, ReserveConfirmed(connectionId, initialReserveMessage.body.criteria)))
    }

    "send PassedEndTime message after reservation end time" in new SingleConnectionActorFixture {
      reserveWithEndTime(DateTime.now())

      eventually(query.getConnectionStates().getLifecycleState() must_== LifecycleStateEnumType.PASSED_END_TIME)
    }

    "ignore PassedEndTime message received before end time" in new SingleConnectionActorFixture {
      reserveWithEndTime(DateTime.now().plusDays(1))

      await(connection ? PassedEndTime(newCorrelationId, connectionId, DateTime.now()))

      query.getConnectionStates().getLifecycleState() must_== LifecycleStateEnumType.CREATED
    }
  }
}
