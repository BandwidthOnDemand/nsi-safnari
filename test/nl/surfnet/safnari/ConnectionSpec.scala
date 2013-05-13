package nl.surfnet.safnari

import java.util.UUID
import java.net.URI
import scala.concurrent.duration._
import akka.testkit._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import org.specs2.mutable.After
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.TypeValuePairListType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends helpers.Specification {

  trait fixture extends After {

    implicit val system = ActorSystem("test-" + UUID.randomUUID().toString)
    implicit val timeout = Timeout(2.seconds)

    override def after = {
      system.shutdown
      system.awaitTermination
    }

    val Criteria = new ReservationConfirmCriteriaType().withSchedule(new ScheduleType()).withBandwidth(100).withServiceAttributes(new TypeValuePairListType()).withPath(new PathType())
    val InitialReserveType = new ReserveType().withCriteria(Criteria)
    val A = ComputedSegment(new StpType().withLocalId("A"), new StpType().withLocalId("X"), ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"), NoAuthentication))
    val B = ComputedSegment(new StpType().withLocalId("X"), new StpType().withLocalId("B"), ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider"), NoAuthentication))

    val Headers = NsiHeaders(newCorrelationId, "RequesterNSA", "ProviderNSA", Some(URI.create("http://example.com/")))
    val ConnectionId = "ConnectionId"
    val ReserveCorrelationId = newCorrelationId
    val InitialMessages = Seq(FromRequester(Reserve(ReserveCorrelationId, InitialReserveType)))

    val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    val PceReplyToUri = URI.create("http://example.com/pce/reply")

    var messages: Seq[Message] = Nil

    val connection = TestFSMRef(new ConnectionActor(ConnectionId, "RequesterNSA", () => CorrelationId.fromUuid(mockUuidGenerator()), TestActorRef(new Actor {
      def receive = { case m: Message => messages = messages :+ m }
    }), PceReplyToUri))

    def given(messages: Message*): Unit = messages.foreach(m => connection ! m)

    def when(message: Message): Message = {
      messages = Nil
      Await.result(connection ? message, Duration.Inf)
    }
  }

  "A connection" should {
    "send a reserve response when reserve is requested" in new fixture {
      val ack = when(FromRequester(Reserve(ReserveCorrelationId, InitialReserveType)))

      ack.asInstanceOf[ReserveResponse].correlationId must beEqualTo(ReserveCorrelationId)
    }

    "send a path computation request when reserve is received" in new fixture {
      when(FromRequester(Reserve(ReserveCorrelationId, InitialReserveType)))

      messages must contain(ToPce(PathComputationRequest(
        correlationId = CorrelationId(0, 1),
        replyTo = PceReplyToUri,
        criteria = Criteria)))
    }

    "send reserve request for each segment when path computation confirmed is received" in new fixture {
      given(InitialMessages: _*)

      when(FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))))

      messages must haveSize(2)
      messages must haveAllElementsLike {
        case ToProvider(reserve: Reserve, A.provider) => ok
        case ToProvider(reserve: Reserve, B.provider) => ok
      }
    }

    "fail the connection when path computation fails" in new fixture {
      given(InitialMessages: _*)

      when(FromPce(PathComputationFailed(CorrelationId(0, 1))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(ReserveCorrelationId, _)) => ok
      }
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A)))): _*)

      when(FromProvider(ReserveConfirmed(CorrelationId(0, 2), "connectionId", Criteria)))

      messages must contain(ToRequester(ReserveConfirmed(ReserveCorrelationId, ConnectionId, Criteria)))

      connection.stateName must beEqualTo(HeldReservationState)
    }

    "be in reservation held state when both segments are confirmed" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B)))): _*)

      when(FromProvider(ReserveConfirmed(CorrelationId(0, 2), "ConnectionIdA", Criteria)))
      messages must beEmpty

      when(FromProvider(ReserveConfirmed(CorrelationId(0, 3), "ConnectionIdB", Criteria)))
      messages must contain(ToRequester(ReserveConfirmed(ReserveCorrelationId, ConnectionId, Criteria)))

      connection.stateName must beEqualTo(HeldReservationState)
    }

    "fail the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A)))): _*)

      when(FromProvider(ReserveFailed(CorrelationId(0, 2), new GenericFailedType())))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(ReserveCorrelationId, _)) => ok
      }
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "fail the reservation with two segments and at least one fails" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))),
        FromProvider(ReserveFailed(CorrelationId(0, 2), new GenericFailedType()))): _*)

      when(FromProvider(ReserveConfirmed(CorrelationId(0, 3), "connectionIdB", Criteria)))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(ReserveCorrelationId, _)) => ok
      }
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "be in committing state when reserve commit is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(ReserveConfirmed(CorrelationId(0, 2), ConnectionId, Criteria))): _*)

      when(FromRequester(ReserveCommit(newCorrelationId, ConnectionId)))

      messages must haveOneElementLike { case ToProvider(_: ReserveCommit, _) => ok }
      connection.stateName must beEqualTo(CommittingReservationState)
    }

    "be in reserved state when reserve commit confirmed is received" in new fixture {
      val CommitCorrelationId = newCorrelationId
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(ReserveConfirmed(CorrelationId(0, 2), "ConnectionIdA", Criteria)),
        //ToRequester(ReserveConfirmed(CorrelationId(0, 0), ConnectionId, Criteria)),
        FromRequester(ReserveCommit(CommitCorrelationId, ConnectionId))): _*)

      when(FromProvider(ReserveCommitConfirmed(CorrelationId(0, 3), "ConnectionIdA")))

      messages must contain(ToRequester(ReserveCommitConfirmed(CommitCorrelationId, ConnectionId)))
      connection.stateName must beEqualTo(ReservedReservationState)
    }

    "be in aborting state when reserve abort is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(ReserveConfirmed(CorrelationId(0, 2), "ConnectionIdA", Criteria))): _*)

      when(FromRequester(ReserveAbort(ReserveCorrelationId, ConnectionId)))

      messages must haveOneElementLike { case ToProvider(_: ReserveAbort, _) => ok }
      connection.stateName must beEqualTo(AbortingReservationState)
    }

    "provide information about connections" in new fixture {
      given(InitialMessages: _*)

      val response = when('query)

      response must beLike {
        case result: QuerySummaryResultType =>
          result.getConnectionId() must beEqualTo(ConnectionId)
      }
    }
  }
}
