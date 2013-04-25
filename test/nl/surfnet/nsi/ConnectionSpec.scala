package nl.surfnet.nsi

import java.util.UUID
import java.net.URI
import scala.concurrent.duration._
import akka.testkit._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.specs2.time.NoTimeConversions
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import scala.concurrent.Await
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.specs2.mutable.After
import org.specs2.execute.PendingUntilFixed
import org.ogf.schemas.nsi._2013._04.framework.types.TypeValuePairListType
import java.net.URL

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends org.specs2.mutable.Specification with NoTimeConversions with PendingUntilFixed {

  trait fixture extends After {
    implicit val system = ActorSystem("test-" + UUID.randomUUID().toString)
    implicit val timeout = Timeout(2.seconds)

    override def after = {
      system.shutdown
      system.awaitTermination
    }

    val InitialReserveType = new ReserveType().withCriteria(new ReservationRequestCriteriaType().withSchedule(new ScheduleType()).withBandwidth(100).withServiceAttributes(new TypeValuePairListType()).withPath(new PathType()))
    val A = ComputedSegment(new StpType().withLocalId(""), new StpType().withLocalId(""), "urn:ogf:network:es.net", new URL("http://example.com/provider"), NoAuthentication)
    val B = ComputedSegment(new StpType().withLocalId(""), new StpType().withLocalId(""), "urn:ogf:network:surfnet.nl", new URL("http://excample.com/provider"), NoAuthentication)

    val Headers = NsiHeaders(UUID.randomUUID, "RequesterNSA", "ProviderNSA", Some(URI.create("http://example.com/")))
    val ConnectionId = "ConnectionId"
    val CorrelationId = newCorrelationId
    val InitialMessages = Seq(Inbound(Reserve(CorrelationId, InitialReserveType)))

    var messages: Seq[Message] = Nil

    val connection = TestFSMRef(new ConnectionActor(ConnectionId, "RequesterNSA", Uuid.mockUuidGenerator(1), TestActorRef(new Actor {
      def receive = { case m: Message => messages = messages :+ m }
    })))

    def given(messages: Message*): Unit = messages.foreach(m => connection ! m)

    def when(message: Message): Message = {
      messages = Nil
      Await.result(connection ? message, Duration.Inf)
    }
  }

  "A connection" should {
    "send a reserve response when reserve is requested" in new fixture {
      val ack = when(Inbound(Reserve(CorrelationId, InitialReserveType)))

      ack.asInstanceOf[ReserveResponse].correlationId must beEqualTo(CorrelationId)
    }

    "send a path computation request when reserve is received" in new fixture {
      when(Inbound(Reserve(CorrelationId, InitialReserveType)))

      messages must haveOneElementLike { case request: PathComputationRequest => ok }
    }

    "send reserve requests to when path computation confirmed is received" in new fixture {
      given(InitialMessages: _*)

      when(Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A, B))))

      messages must haveSize(2)
    }

    "fail the connection when path computation fails" in new fixture {
      given(InitialMessages: _*)

      when(Inbound(PathComputationFailed(new UUID(0, 1))))

      messages must contain(ReserveFailed(CorrelationId, ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A)))): _*)

      when(Inbound(ReserveConfirmed(new UUID(0, 2), "connectionId")))

      messages must contain(ReserveConfirmed(CorrelationId, ConnectionId))

      connection.stateName must beEqualTo(HeldReservationState)
    }

    "be in reservation held state when both segments are confirmed" in new fixture {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A, B)))): _*)

      when(Inbound(ReserveConfirmed(new UUID(0, 2), "ConnectionIdA")))
      messages must beEmpty

      when(Inbound(ReserveConfirmed(new UUID(0, 3), "ConnectionIdB")))
      messages must contain(ReserveConfirmed(CorrelationId, ConnectionId))

      connection.stateName must beEqualTo(HeldReservationState)
    }

    "fail the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A)))): _*)

      when(Inbound(ReserveFailed(new UUID(0, 2), ConnectionId)))

      messages must contain(ReserveFailed(CorrelationId, ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "fail the reservation with two segments and at least one fails" in new fixture {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A, B))),
        Inbound(ReserveFailed(new UUID(0, 2), ConnectionId))): _*)

      when(Inbound(ReserveConfirmed(new UUID(0, 3), "connectionIdB")))

      messages must contain(ReserveFailed(CorrelationId, ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "be in committing state when reserve commit is received" in new fixture {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A))),
        Inbound(ReserveConfirmed(new UUID(0, 2), ConnectionId))): _*)

      when(Inbound(ReserveCommit(newCorrelationId, ConnectionId)))

      messages must haveOneElementLike { case request: ReserveCommit => ok }
      connection.stateName must beEqualTo(CommittingReservationState)
    }

    "be in reserved state when reserve commit confirmed is received" in new fixture {
      val CommitCorrelationId = newCorrelationId
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A))),
        Inbound(ReserveConfirmed(new UUID(0, 2), "ConnectionIdA")),
        Inbound(ReserveCommit(CommitCorrelationId, ConnectionId))): _*)

      when(Inbound(ReserveCommitConfirmed(new UUID(0, 3), "ConnectionIdA")))

      messages must contain(ReserveCommitConfirmed(CommitCorrelationId, ConnectionId))
      connection.stateName must beEqualTo(ReservedReservationState)
    }

    "be in aborting state when reserve abort is received" in new fixture {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(new UUID(0, 1), Seq(A))),
        Inbound(ReserveConfirmed(new UUID(0, 2), "ConnectionIdA"))): _*)

      when(Inbound(ReserveAbort(CorrelationId, ConnectionId)))

      messages must haveOneElementLike { case request: ReserveAbort => ok }
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
