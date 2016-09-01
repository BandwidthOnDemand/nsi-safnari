package controllers

import java.net.URI

import akka.actor.Actor
import akka.testkit.TestActorRef
import controllers.Connection.Delete
import nl.surfnet.nsiv2.messages.CorrelationId
import nl.surfnet.nsiv2.utils._
import nl.surfnet.safnari._
import org.joda.time.{DateTime, Instant}
import org.ogf.schemas.nsi._2013._12.connection.types._
import play.api.libs.concurrent.Akka
import play.api.test._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionManagerSpec extends helpers.Specification {
  sequential

  import nl.surfnet.nsiv2.messages._
  import NsiMessageSpec._

  class RecordingActor extends Actor {
    @volatile var messages = Vector.empty[Any]

    def receive = {
      case m =>
        messages :+= m
    }
  }

  private def command(message: Message, timestamp: Instant = new Instant()) = Connection.Command(timestamp, message)

  abstract class DummyConnectionFixture extends WithApplication() {
    implicit lazy val system = Akka.system

    lazy val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    lazy val outbound = TestActorRef(new RecordingActor)

    lazy val connectionManager = new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, AggregatorNsa, ChainAlgorithm, URI.create("http://localhost"), URI.create("http://localhost"))))
  }

  "Connection manager" should {
    "find a created connection" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)
      val data = await(actor ? Connection.Query).summary

      connectionManager.get(data.getConnectionId) must beSome(actor)
    }

    "add child connection id" in new DummyConnectionFixture {
      implicit val sender = Actor.noSender
      val initialReserve = ura.request(CorrelationId(0, 0), initialReserveMessage.body)
      val Some(connection) = connectionManager.findOrCreateConnection(initialReserve.message.asInstanceOf[NsiProviderMessage[NsiProviderCommand]])
      connection ! command(initialReserve)
      connection ! command(pce.confirm(CorrelationId(0, 1), A))
      await(connection ? command(upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnectionId", ConfirmCriteria))))

      connectionManager.findByChildConnectionId("ChildConnectionId") must beSome(connection)
    }

    "add child connection id during replay" in new DummyConnectionFixture {
      val Some(actor) = connectionManager.findOrCreateConnection(initialReserveMessage)

      await(actor ? Connection.Replay(Seq(
        command(FromRequester(initialReserveMessage)),
        command(pce.confirm(CorrelationId(0, 1), A)),
        command(upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria))))))

      connectionManager.findByChildConnectionId("ChildConnection") must beSome(actor)
    }

    "find connection by requester correlationId for idempotent command handling" in new DummyConnectionFixture {
      val Some(actor1) = connectionManager.findOrCreateConnection(initialReserveMessage)
      val Some(actor2) = connectionManager.findOrCreateConnection(initialReserveMessage)

      actor1 must_== actor2
    }

    "accept duplicate globalReservationId" in new DummyConnectionFixture {
      def newReserveWithGlobalReservationId(globalReservationId: String) = {
        val message = initialReserveMessage.tap(_.body.body.setGlobalReservationId("urn:A"))
        message.copy(message.headers.copy(correlationId = helpers.Specification.newCorrelationId))
      }
      val Some(connection1) = connectionManager.findOrCreateConnection(newReserveWithGlobalReservationId("urn:A"))
      val Some(connection2) = connectionManager.findOrCreateConnection(newReserveWithGlobalReservationId("urn:A"))

      connectionManager.findByGlobalReservationIds(Seq(URI.create("urn:A"))) must beEqualTo(Seq(connection1, connection2))
    }

    "delete single connection from globalReservationIdMap" in new DummyConnectionFixture {
      implicit val sender = Actor.noSender

      def newReserveWithGlobalReservationId(globalReservationId: String) = {
        val message = initialReserveMessage.tap(_.body.body.setGlobalReservationId("urn:A"))
        message.copy(message.headers.copy(correlationId = helpers.Specification.newCorrelationId))
      }
      val Some(connection1) = connectionManager.findOrCreateConnection(newReserveWithGlobalReservationId("urn:A"))
      val Some(connection2) = connectionManager.findOrCreateConnection(newReserveWithGlobalReservationId("urn:A"))

      connection1 ! Delete

      connectionManager.findByGlobalReservationIds(Seq(URI.create("urn:A"))) must beEqualTo(Seq(connection2)).eventually
    }
  }

  class SingleConnectionActorFixture(additionalConfiguration: Map[String, Any] = Map.empty) extends WithApplication(FakeApplication(additionalConfiguration = additionalConfiguration)) {
    implicit lazy val system = Akka.system
    implicit lazy val executionContext = system.dispatcher

    def createConnectionManager = {
      val mockUuidGenerator = Uuid.mockUuidGenerator(1)
      def newCorrelationId = CorrelationId.fromUuid(mockUuidGenerator())
      new ConnectionManager((id, reserve) => (outbound, new ConnectionEntity(id, reserve, () => newCorrelationId, AggregatorNsa, ChainAlgorithm, URI.create("http://localhost"), URI.create("http://localhost"))))
    }

    lazy val outbound = TestActorRef(new RecordingActor)
    lazy val connectionManager = createConnectionManager
    lazy val Some((connectionId, connection)) = {
      connectionManager.findOrCreateConnection(initialReserveMessage).map { c =>
        (await(c ? Connection.Query).summary.getConnectionId(), c)
      }
    }

    def query = await(connection ? Connection.Query).summary

    def reserveWithEndTime(endTime: DateTime): Unit = {
      val reserve = initialReserveMessage.tap(_.body.body.getCriteria().getSchedule().withEndTime(endTime.toXmlGregorianCalendar))
      val messages = Seq(
        FromRequester(reserve),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)),
        ura.request(CorrelationId(1, 1), ReserveCommit(connectionId)),
        upa.response(CorrelationId(0, 4), ReserveCommitConfirmed("ChildConnection")))

      messages.foreach { message =>
        await(connection ? command(message))
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
      val messages = Seq(FromRequester(initialReserveMessage), pce.confirm(CorrelationId(0, 1), A), upa.response(CorrelationId(0, 1), ReserveConfirmed("ChildConnection", ConfirmCriteria)))
      messages.foreach(message => await(connection ? command(message)))

      val originalQueryResult = await(connection ? Connection.Query)
      system.stop(connection.actor)

      val restoredConnectionManager = createConnectionManager
      await(restoredConnectionManager.restore)

      val restoredConnection = restoredConnectionManager.get(connectionId)

      restoredConnection aka "restored connection" must beSome
      await(restoredConnection.get ? Connection.Query) must_== originalQueryResult
    }

    "ensure retransmitted message is exactly the same as the original" in new SingleConnectionActorFixture {
      val OriginalReserve = InitialReserveType.withDescription("original")
      val ModifiedReserve = InitialReserveType.withDescription("modified")

      val ack = await(connection ? command(FromRequester(NsiProviderMessage(initialReserveMessage.headers, InitialReserve(OriginalReserve)))))
      val error = await(connection ? command(FromRequester(NsiProviderMessage(initialReserveMessage.headers, InitialReserve(ModifiedReserve)))))

      error must beLike {
        case ServiceException(details) =>
          details must_== NsiError.GenericMessagePayloadError.toServiceException(AggregatorNsa).withText(s"duplicate request with existing correlation id ${initialReserveMessage.headers.correlationId} does not match the original")
      }
    }

    "retransmit original async reply on duplicate request" in new SingleConnectionActorFixture {
      val ack = await(connection ? command(FromRequester(initialReserveMessage)))
      await(connection ? command(pce.confirm(CorrelationId(0, 1), A)))

      val original = output {
        await(connection ? command(upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria))))
      }

      val retransmitted = output {
        await(connection ? command(FromRequester(initialReserveMessage)))
      }

      retransmitted must_== original
    }

    "retransmit PCE message when no async reply has been received" in new SingleConnectionActorFixture {
      val original = output {
        await(connection ? command(FromRequester(initialReserveMessage)))
      }

      val retransmitted = output {
        await(connection ? command(FromRequester(initialReserveMessage)))
      }

      retransmitted aka "retransmitted" must_== original
    }

    "retransmit ToProvider messages without async reply" in new SingleConnectionActorFixture {
      await(connection ? command(FromRequester(initialReserveMessage)))
      val original = output {
        await(connection ? command(pce.confirm(CorrelationId(0, 1), A, B)))
      }
      val retransmitted = output {
        await(connection ? command(FromRequester(initialReserveMessage)))
      }
      retransmitted aka "retransmitted" must_== original

      await(connection ? command(upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnectionA", ConfirmCriteria))))

      val retransmitted2 = output {
        await(connection ? command(FromRequester(initialReserveMessage)))
      }

      retransmitted2 aka "retransmitted 2" must_== (original.collect {
        case message @ ToProvider(NsiProviderMessage(_, _), provider) if provider == B.provider => message
      })
    }

    "restore state from replayed messages" in new SingleConnectionActorFixture {
      await(connection ? Connection.Replay(Seq(
        command(FromRequester(initialReserveMessage)),
        command(pce.confirm(CorrelationId(0, 1), A)),
        command(upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria))))))

      val retransmitted = output {
        await(connection ? command(FromRequester(initialReserveMessage)))
      }

      retransmitted must_== Vector(agg.response(initialReserveMessage.headers.correlationId, ReserveConfirmed(connectionId, ConfirmCriteria)))
    }

    "send PassedEndTime message after reservation end time" in new SingleConnectionActorFixture {
      reserveWithEndTime(DateTime.now())

      eventually(query.getConnectionStates().getLifecycleState() must_== LifecycleStateEnumType.PASSED_END_TIME)
    }

    "support PassedEndTime more than 248 days into the future" in new SingleConnectionActorFixture {
      reserveWithEndTime(DateTime.now().plusDays(300))
    }

    "ignore PassedEndTime message received before end time" in new SingleConnectionActorFixture {
      reserveWithEndTime(DateTime.now().plusDays(1))

      await(connection ? command(PassedEndTime(newCorrelationId, connectionId, DateTime.now())))

      query.getConnectionStates().getLifecycleState() must_== LifecycleStateEnumType.CREATED
    }

    "be deleted after a grace period" should {
      "when never successfully reserved" in new SingleConnectionActorFixture() {
        await(connection ? command(ura.request(CorrelationId(0, 0), initialReserveMessage.body), timestamp = new Instant().minus(Configuration.ConnectionExpirationTime.toMillis)))

        eventually(connectionManager.get(connectionId) must beNone)
        connectionManager.messageStore.loadEverything().map(_._1) must not(contain(connectionId))
      }

      "delete connections with LSM state different from CREATED after a grace period" in new SingleConnectionActorFixture(additionalConfiguration = Map("safnari.connection.expiration.time" -> "250 milliseconds")) {
        reserveWithEndTime(DateTime.now())

        eventually(connectionManager.get(connectionId) must beNone)
      }
    }
  }
}
