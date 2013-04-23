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
import org.specs2.execute.PendingUntilFixed
import org.specs2.execute.PendingUntilFixed

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends org.specs2.mutable.Specification with NoTimeConversions with PendingUntilFixed {
  implicit val system = ActorSystem("test")
  implicit val timeout = Timeout(2.seconds)

  isolated

  val Headers = NsiHeaders(UUID.randomUUID, Some(URI.create("http://example.com/")))
  val ConnectionId = "ConnectionId"
  val CorrelationId = newCorrelationId
  val InitialMessages = Seq(Inbound(Reserve(CorrelationId)))

  var messages: Seq[Message] = Nil

  val connection = TestFSMRef(new ConnectionActor(ConnectionId, TestActorRef(new Actor {
    def receive = { case m: Message => messages = messages :+ m }})))

  def given(messages: Message*): Unit = messages.foreach(m => connection ! m)

  def when(message: Message): Message = {
    messages = Nil
    Await.result(connection ? message, Duration.Inf)
  }

  "A connection" should {
    "send a reserve response when reserve is requested" in {
      val ack = when(Inbound(Reserve(CorrelationId)))

      ack.asInstanceOf[ReserveResponse].correlationId must beEqualTo(CorrelationId)
    }

    "send a path computation request when reserve is received" in {
      when(Inbound(Reserve(CorrelationId)))

      messages must haveOneElementLike { case request: PathComputationRequest => ok }
    }

    "send reserve requests to when path computation confirmed is received" in {
      given(InitialMessages: _*)

      when(Inbound(PathComputationConfirmed(CorrelationId, Seq("A", "B"))))

      messages must haveSize(2)
    }

    "fail the connection when path computation fails" in {
      given(InitialMessages: _*)

      when(Inbound(PathComputationFailed(CorrelationId)))

      messages must contain(ReserveFailed(CorrelationId, ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(CorrelationId, "SegmentConnectionId"))): _*)

      when(Inbound(ReserveConfirmed(CorrelationId, "connectionId")))

      messages must contain(ReserveConfirmed(CorrelationId, ConnectionId))

      connection.stateName must beEqualTo(HeldReservationState)
    }

    "be in reservation held state when both segments are confirmed" in {
      given(InitialMessages ++ Seq(
          Inbound(PathComputationConfirmed(CorrelationId, Seq("A", "B"))),
          Inbound(ReserveResponse(CorrelationId, "ConnectionIdA")),
          Inbound(ReserveResponse(CorrelationId, "ConnectionIdB"))): _*)

      when(Inbound(ReserveConfirmed(CorrelationId, "ConnectionIdA")))
      messages must beEmpty

      when(Inbound(ReserveConfirmed(CorrelationId, "ConnectionIdB")))
      messages must contain(ReserveConfirmed(CorrelationId, ConnectionId))

      connection.stateName must beEqualTo(HeldReservationState)
    }

    "fail the reservation with a single path segment" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(CorrelationId, "SegmentConnectionId"))): _*)

      when(Inbound(ReserveFailed(CorrelationId, ConnectionId)))

      messages must contain(ReserveFailed(CorrelationId, ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "fail the reservation with two segments and at least one fails" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A", "B"))),
        Inbound(ReserveResponse(CorrelationId, "SegmentConnectionId")),
        Inbound(ReserveFailed(CorrelationId, ConnectionId))): _*)

     when(Inbound(ReserveConfirmed(CorrelationId, "connectionIdA")))

     messages must contain(ReserveFailed(CorrelationId, ConnectionId))
     connection.stateName must beEqualTo(FailedReservationState)
    }.pendingUntilFixed

    "be in committing state when reserve commit is received" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(CorrelationId, "SegmentConnectionId")),
        Inbound(ReserveConfirmed(CorrelationId, ConnectionId))): _*)

      when(Inbound(ReserveCommit(CorrelationId, "SegmentConnectionId")))

      messages must haveOneElementLike { case request: ReserveCommit => ok }
      connection.stateName must beEqualTo(CommittingReservationState)
    }

    "be in reserved state when reserve commit confirmed is received" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(CorrelationId, "SegmentConnectionId")),
        Inbound(ReserveConfirmed(CorrelationId, ConnectionId)),
        Inbound(ReserveCommit(CorrelationId, ConnectionId))): _*)

      when(Inbound(ReserveCommitConfirmed(CorrelationId, "SegmentConnectionId")))

      messages must contain(ReserveCommitConfirmed(CorrelationId, ConnectionId))
      connection.stateName must beEqualTo(ReservedReservationState)
    }

    "be in aborting state when reserve abort is received" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(CorrelationId, "SegmentConnectionId")),
        Inbound(ReserveConfirmed(CorrelationId, ConnectionId))): _*)

      when(Inbound(ReserveAbort(CorrelationId, "SegmentConnectionId")))

      messages must haveOneElementLike { case request: ReserveAbort => ok }
      connection.stateName must beEqualTo(AbortingReservationState)
    }

  }
}
