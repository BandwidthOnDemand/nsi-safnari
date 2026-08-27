package controllers

import controllers.Connection.Delete
import helpers.NsiMessages.*
import java.net.URI
import java.time.Instant
import java.time.temporal.*
import java.util.UUID
import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.utils.*
import nl.surfnet.safnari.*
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.{Actor, ActorSystem}
import org.apache.pekko.testkit.TestActorRef
import org.ogf.schemas.nsi._2013._12.connection.types.*
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.*
import scala.concurrent.ExecutionContext

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionManagerSpec extends helpers.Specification:
  sequential

  private def FakeApplication(additionalConfiguration: Map[String, Any]) =
    new GuiceApplicationBuilder().configure(additionalConfiguration).build()

  class RecordingActor extends Actor:
    @volatile var messages: Vector[Any] = Vector.empty[Any]

    def receive: PartialFunction[Any, Unit] = { case m =>
      messages :+= m
    }

  private def command(
      message: Message,
      timestamp: Instant = Instant.ofEpochMilli(System.currentTimeMillis)
  ) = Connection.Command(timestamp, message)

  abstract class DummyConnectionFixture extends WithApplication():
    lazy val messageStore: SafnariMessageStore = app.injector.instanceOf[SafnariMessageStore]
    lazy val configuration: Configuration = app.injector.instanceOf[Configuration]
    implicit lazy val system: ActorSystem = app.injector.instanceOf[ActorSystem]

    lazy val mockUuidGenerator: () => UUID = Uuid.mockUuidGenerator(1)
    def newCorrelationId(): CorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    lazy val mockQueryUuidGenerator: () => UUID = Uuid.mockUuidGenerator(1000)
    def newQueryCorrelationId(): CorrelationId = CorrelationId.fromUuid(mockQueryUuidGenerator())

    lazy val outbound: TestActorRef[RecordingActor] = TestActorRef(new RecordingActor)

    lazy val connectionManager = new ConnectionManager(
      (id, reserve, _) =>
        (
          outbound,
          new ConnectionEntity(
            AggregatorNsa,
            id,
            reserve,
            () => newCorrelationId(),
            () => newQueryCorrelationId(),
            PathComputationAlgorithm.CHAIN,
            URI.create("http://localhost"),
            URI.create("http://localhost")
          )
        ),
      configuration,
      messageStore
    )
  end DummyConnectionFixture

  "Connection manager" should {
    "find a created connection" in new DummyConnectionFixture:
      override def running() =
        val actor = connectionManager.createConnection(newConnectionId(), initialReserveMessage)
        val data = await(actor ? Connection.Query).summary

        connectionManager.get(data.getConnectionId) must beSome(actor)

    "add child connection id" in new DummyConnectionFixture:
      override def running() =
        implicit val sender: ActorRef = Actor.noSender
        val initialReserve = ura.request(CorrelationId(0, 0), initialReserveMessage.body)
        val connection = connectionManager.createConnection(
          newConnectionId(),
          initialReserve.message.asInstanceOf[NsiProviderMessage[InitialReserve]]
        )
        connection ! command(initialReserve)
        connection ! command(pce.confirm(CorrelationId(0, 1), A))
        await(
          connection ? command(
            upa.response(
              CorrelationId(0, 2),
              ReserveConfirmed("ChildConnectionId", ConfirmCriteria)
            )
          )
        )

        connectionManager.findByChildConnectionId("ChildConnectionId") must beSome(connection)

    "add child connection id during replay" in new DummyConnectionFixture:
      override def running() =
        val actor = connectionManager.createConnection(newConnectionId(), initialReserveMessage)

        await(
          actor ? Connection.Replay(
            Seq(
              command(FromRequester(initialReserveMessage)),
              command(pce.confirm(CorrelationId(0, 1), A)),
              command(
                upa.response(
                  CorrelationId(0, 2),
                  ReserveConfirmed("ChildConnection", ConfirmCriteria)
                )
              )
            )
          )
        )

        connectionManager.findByChildConnectionId("ChildConnection") must beSome(actor)

    "find connection by requester correlationId for idempotent command handling" in new DummyConnectionFixture:
      override def running() =
        val actor1 = connectionManager.findOrCreateConnection(initialReserveMessage)
        val actor2 = connectionManager.findOrCreateConnection(initialReserveMessage)

        actor1 must beSome
        actor1 must_== actor2

    "accept duplicate globalReservationId" in new DummyConnectionFixture:
      override def running() =
        def newReserveWithGlobalReservationId(globalReservationId: String) =
          val message =
            initialReserveMessage.tap(_.body.body.setGlobalReservationId(globalReservationId))
          message.copy(
            message.headers.copy(correlationId = helpers.Specification.newCorrelationId())
          )
        val connection1 = connectionManager.createConnection(
          newConnectionId(),
          newReserveWithGlobalReservationId("urn:A")
        )
        val connection2 = connectionManager.createConnection(
          newConnectionId(),
          newReserveWithGlobalReservationId("urn:A")
        )

        connectionManager.findByGlobalReservationIds(Seq(URI.create("urn:A"))) must beEqualTo(
          Seq(connection1, connection2)
        )

    "delete single connection from globalReservationIdMap" in new DummyConnectionFixture:
      override def running() =
        implicit val sender: ActorRef = Actor.noSender

        def newReserveWithGlobalReservationId(globalReservationId: String) =
          val message =
            initialReserveMessage.tap(_.body.body.setGlobalReservationId(globalReservationId))
          message.copy(
            message.headers.copy(correlationId = helpers.Specification.newCorrelationId())
          )
        val connection1 = connectionManager.createConnection(
          newConnectionId(),
          newReserveWithGlobalReservationId("urn:A")
        )
        val connection2 = connectionManager.createConnection(
          newConnectionId(),
          newReserveWithGlobalReservationId("urn:A")
        )

        connection1 ! Delete

        connectionManager.findByGlobalReservationIds(Seq(URI.create("urn:A"))) must beEqualTo(
          Seq(connection2)
        ).eventually
      end running
  }

  class SingleConnectionActorFixture(additionalConfiguration: Map[String, Any] = Map.empty)
      extends WithApplication(FakeApplication(additionalConfiguration = additionalConfiguration)):
    lazy val messageStore: SafnariMessageStore = app.injector.instanceOf[SafnariMessageStore]
    lazy val configuration: Configuration = app.injector.instanceOf[Configuration]

    implicit lazy val system: ActorSystem = app.injector.instanceOf[ActorSystem]
    implicit lazy val executionContext: ExecutionContext = system.dispatcher

    def createConnectionManager(): ConnectionManager =
      val mockUuidGenerator = Uuid.mockUuidGenerator(1)
      def newCorrelationId() = CorrelationId.fromUuid(mockUuidGenerator())
      val mockQueryUuidGenerator = Uuid.mockUuidGenerator(1000)
      def newQueryCorrelationId() = CorrelationId.fromUuid(mockQueryUuidGenerator())
      new ConnectionManager(
        (id, reserve, _) =>
          (
            outbound,
            new ConnectionEntity(
              AggregatorNsa,
              id,
              reserve,
              () => newCorrelationId(),
              () => newQueryCorrelationId(),
              PathComputationAlgorithm.CHAIN,
              URI.create("http://localhost"),
              URI.create("http://localhost")
            )
          ),
        configuration,
        messageStore
      )
    end createConnectionManager

    lazy val outbound: TestActorRef[RecordingActor] = TestActorRef(new RecordingActor)
    lazy val connectionManager: ConnectionManager = createConnectionManager()
    lazy val connectionId: ConnectionId = newConnectionId()
    lazy val connection: Connection =
      connectionManager.createConnection(connectionId, initialReserveMessage)

    def query(): QuerySummaryResultType = await(connection ? Connection.Query).summary

    def reserveWithEndTime(endTime: Instant): Unit =
      val reserve = initialReserveMessage.tap(
        _.body.body.getCriteria().getSchedule().withEndTime(endTime.toXMLGregorianCalendar())
      )
      val messages = Seq(
        FromRequester(reserve),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)),
        ura.request(CorrelationId(1, 1), ReserveCommit(connectionId)),
        upa.response(CorrelationId(0, 4), ReserveCommitConfirmed("ChildConnection"))
      )

      messages.foreach { message =>
        await(connection ? command(message))
      }

    def output[A](f: => A): Vector[Any] =
      outbound.underlyingActor.messages = Vector.empty
      f
      outbound.underlyingActor.messages
  end SingleConnectionActorFixture

  "Connection actor" should {
    "persist and restore from the database" in new SingleConnectionActorFixture:
      override def running() =
        val messages = Seq(
          FromRequester(initialReserveMessage),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 1), ReserveConfirmed("ChildConnection", ConfirmCriteria))
        )
        messages.foreach(message => await(connection ? command(message)))

        val originalQueryResult = await(connection ? Connection.Query)
        system.stop(connection.actor)

        val restoredConnectionManager = createConnectionManager()
        await(restoredConnectionManager.restore)

        val restoredConnection = restoredConnectionManager.get(connectionId)

        restoredConnection aka "restored connection" must beSome
        await(restoredConnection.get ? Connection.Query) must_== originalQueryResult

    "ensure retransmitted message is exactly the same as the original" in new SingleConnectionActorFixture:
      override def running() =
        val OriginalReserve = InitialReserveType.withDescription("original")
        val ModifiedReserve = InitialReserveType.withDescription("modified")

        val ack = await(
          connection ? command(
            FromRequester(
              NsiProviderMessage(initialReserveMessage.headers, InitialReserve(OriginalReserve))
            )
          )
        )
        ack must beAnInstanceOf[ReserveResponse]

        val error = await(
          connection ? command(
            FromRequester(
              NsiProviderMessage(initialReserveMessage.headers, InitialReserve(ModifiedReserve))
            )
          )
        )

        error must beLike { case ServiceException(details) =>
          details must_== NsiError.GenericMessagePayloadError
            .toServiceException(AggregatorNsa)
            .withText(
              s"duplicate request with existing correlation id ${initialReserveMessage.headers.correlationId} does not match the original"
            )
        }
      end running

    "retransmit original async reply on duplicate request" in new SingleConnectionActorFixture:
      override def running() =
        val ack = await(connection ? command(FromRequester(initialReserveMessage)))
        ack must beAnInstanceOf[ReserveResponse]

        await(connection ? command(pce.confirm(CorrelationId(0, 1), A)))

        val original = output {
          await(
            connection ? command(
              upa.response(
                CorrelationId(0, 2),
                ReserveConfirmed("ChildConnection", ConfirmCriteria)
              )
            )
          )
        }

        val retransmitted = output {
          await(connection ? command(FromRequester(initialReserveMessage)))
        }

        retransmitted must_== original
      end running

    "retransmit PCE message when no async reply has been received" in new SingleConnectionActorFixture:
      override def running() =
        val original = output {
          await(connection ? command(FromRequester(initialReserveMessage)))
        }

        val retransmitted = output {
          await(connection ? command(FromRequester(initialReserveMessage)))
        }

        retransmitted aka "retransmitted" must_== original

    "retransmit ToProvider messages without async reply" in new SingleConnectionActorFixture:
      override def running() =
        await(connection ? command(FromRequester(initialReserveMessage)))
        val original = output {
          await(connection ? command(pce.confirm(CorrelationId(0, 1), A, B)))
        }
        val retransmitted = output {
          await(connection ? command(FromRequester(initialReserveMessage)))
        }
        retransmitted aka "retransmitted" must_== original

        await(
          connection ? command(
            upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnectionA", ConfirmCriteria))
          )
        )

        val retransmitted2 = output {
          await(connection ? command(FromRequester(initialReserveMessage)))
        }

        retransmitted2 aka "retransmitted 2" must_== (original.collect {
          case message @ ToProvider(NsiProviderMessage(_, _), provider) if provider == B.provider =>
            message
        })
      end running

    "restore state from replayed messages" in new SingleConnectionActorFixture:
      override def running() =
        await(
          connection ? Connection.Replay(
            Seq(
              command(FromRequester(initialReserveMessage)),
              command(pce.confirm(CorrelationId(0, 1), A)),
              command(
                upa.response(
                  CorrelationId(0, 2),
                  ReserveConfirmed("ChildConnection", ConfirmCriteria)
                )
              )
            )
          )
        )

        val retransmitted = output {
          await(connection ? command(FromRequester(initialReserveMessage)))
        }

        retransmitted must_== Vector(
          agg.response(
            initialReserveMessage.headers.correlationId,
            ReserveConfirmed(connectionId, ConfirmCriteria),
            any = pathTrace(
              AggregatorNsa,
              connectionId,
              (A.provider.nsa, "ChildConnection") -> Nil
            ) :: Nil
          )
        )
      end running

    "use distinct provider correlation ids for colliding connection hashes" in new SingleConnectionActorFixture:
      override def running() =
        val firstId = "eadafc81-0a10-4b4c-b4a4-1a87ab3c7c6f"
        val secondId = "024de33b-3866-47ac-9ad3-425001a63a42"
        val firstReserve = initialReserveMessage.copy(
          headers = initialReserveMessage.headers.copy(correlationId = CorrelationId(9, 1))
        )
        val secondReserve = initialReserveMessage.copy(
          headers = initialReserveMessage.headers.copy(correlationId = CorrelationId(9, 2))
        )
        val outbound = TestActorRef(new RecordingActor)

        def manager = new ConnectionManager(
          (id, reserve, version) =>
            val generator = version match
              case 1 => Uuid.deterministicUuidGenerator(id.##)
              case 2 => Uuid.nameBasedUuidGenerator(id)
            (
              outbound,
              new ConnectionEntity(
                AggregatorNsa,
                id,
                reserve,
                () => CorrelationId.fromUuid(generator()),
                () => CorrelationId.fromUuid(UUID.randomUUID()),
                PathComputationAlgorithm.CHAIN,
                URI.create("http://localhost"),
                URI.create("http://localhost")
              )
            )
          ,
          configuration,
          messageStore
        )
        val connectionManager = manager
        val first = connectionManager.createConnection(firstId, firstReserve)
        val second = connectionManager.createConnection(secondId, secondReserve)
        def providerIds = outbound.underlyingActor.messages.collect {
          case ToProvider(NsiProviderMessage(headers, _: InitialReserve), _) =>
            headers.correlationId
        }

        firstId.## must beEqualTo(secondId.##)
        outbound.underlyingActor.messages = Vector.empty
        await(first ? command(FromRequester(firstReserve)))
        await(first ? command(pce.confirm(CorrelationId(0, 1), A)))
        val firstIds = providerIds
        outbound.underlyingActor.messages = Vector.empty
        await(second ? command(FromRequester(secondReserve)))
        await(second ? command(pce.confirm(CorrelationId(0, 1), A)))
        val secondIds = providerIds

        firstIds must haveSize(1)
        secondIds must haveSize(1)
        firstIds.toSet.intersect(secondIds.toSet) must beEmpty
      end running

    "not send a provider a correlation id already used by a non-persisted queryRecursive after restart" in new SingleConnectionActorFixture:
      override def running() =
        // Reserve and commit so the connection has a confirmed child segment.
        val setup = Seq(
          FromRequester(initialReserveMessage),
          pce.confirm(CorrelationId(0, 1), A),
          upa.response(CorrelationId(0, 2), ReserveConfirmed("ChildConnection", ConfirmCriteria)),
          ura.request(CorrelationId(1, 1), ReserveCommit(connectionId)),
          upa.response(CorrelationId(0, 4), ReserveCommitConfirmed("ChildConnection"))
        )
        setup.foreach(message => await(connection ? command(message)))

        // queryRecursive is NOT persisted, yet it draws a correlation id from the
        // per-connection generator and sends it to the child provider.
        val queryCorrelationIds = output {
          connection.actor.tell(
            Connection.QueryRecursive(
              ura.request(
                helpers.Specification.newCorrelationId(),
                QueryRecursive(Some(Left(connectionId :: Nil)), None)
              )
            ),
            Actor.noSender
          )
          await(connection ? Connection.Query)
        }.collect { case ToProvider(NsiProviderMessage(headers, _: QueryRecursive), _) =>
          headers.correlationId
        }

        queryCorrelationIds aka "queryRecursive correlation ids sent to provider" must haveSize(1)

        // Restart: rebuild state by replaying only the persisted messages. The queryRecursive
        // above was never persisted, so the regenerated correlation id sequence is now shifted.
        system.stop(connection.actor)
        val restoredConnectionManager = createConnectionManager()
        await(restoredConnectionManager.restore)
        val restoredConnection = restoredConnectionManager.get(connectionId).get

        // Provision after the restart draws the next correlation id from the now out-of-phase
        // sequence and sends it to the same provider.
        val provisionCorrelationIds = output {
          await(
            restoredConnection ? command(
              ura.request(helpers.Specification.newCorrelationId(), Provision(connectionId))
            )
          )
        }.collect { case ToProvider(NsiProviderMessage(headers, _: Provision), _) =>
          headers.correlationId
        }

        provisionCorrelationIds aka "provision correlation ids sent to provider" must haveSize(1)

        // The ultimate provider must never receive the same correlation id twice.
        provisionCorrelationIds.toSet
          .intersect(
            queryCorrelationIds.toSet
          ) aka "correlation ids reused across restart" must beEmpty
      end running

    "send PassedEndTime message after reservation end time" in new SingleConnectionActorFixture:
      override def running() =
        reserveWithEndTime(Instant.now())

        eventually(
          query()
            .getConnectionStates()
            .getLifecycleState() must_== LifecycleStateEnumType.PASSED_END_TIME
        )

    "support PassedEndTime more than 248 days into the future" in new SingleConnectionActorFixture:
      override def running() =
        reserveWithEndTime(Instant.now().plus(300, ChronoUnit.DAYS))

    "ignore PassedEndTime message received before end time" in new SingleConnectionActorFixture:
      override def running() =
        reserveWithEndTime(Instant.now().plus(1, ChronoUnit.DAYS))

        await(connection ? command(PassedEndTime(newCorrelationId(), connectionId, Instant.now())))

        query().getConnectionStates().getLifecycleState() must_== LifecycleStateEnumType.CREATED

    "be deleted after a grace period" should {
      "when never successfully reserved" in new SingleConnectionActorFixture():
        override def running() =
          await(
            connection ? command(
              ura.request(CorrelationId(0, 0), initialReserveMessage.body),
              timestamp = Instant
                .now()
                .minus(configuration.ConnectionExpirationTime.toMillis, ChronoUnit.MILLIS)
            )
          )

          eventually(connectionManager.get(connectionId) must beNone)
          connectionManager.messageStore.loadEverything().map(_._1) must not(
            contain(===(connectionId))
          )

      "delete connections with LSM state different from CREATED after a grace period" in new SingleConnectionActorFixture(
        additionalConfiguration = Map("safnari.connection.expiration.time" -> "250 milliseconds")
      ):
        override def running() =
          reserveWithEndTime(Instant.now())

          eventually(connectionManager.get(connectionId) must beNone)
    }
  }
end ConnectionManagerSpec
