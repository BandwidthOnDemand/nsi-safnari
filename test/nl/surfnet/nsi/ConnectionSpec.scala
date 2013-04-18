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
  val InitialMessages = Seq(Inbound(Reserve(Headers)))

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
      val ack = when(Inbound(Reserve(Headers)))

      ack.asInstanceOf[ReserveResponse].headers must beEqualTo(Headers.copy(replyTo = None))
    }

    "send a path computation request when reserve is received" in {
      when(Inbound(Reserve(Headers)))

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

      messages must contain(ReserveFailed(Headers.copy(replyTo = None), ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(Headers, "SegmentConnectionId"))): _*)

      when(Inbound(ReserveConfirmed(Headers, "connectionId")))

      messages must contain(ReserveConfirmed(Headers.asReply, ConnectionId))
    }

    "reserve two segments and be reserved" in {
      given(InitialMessages ++ Seq(
          Inbound(PathComputationConfirmed(CorrelationId, Seq("A", "B"))),
          Inbound(ReserveResponse(Headers, "ConnectionIdA")),
          Inbound(ReserveResponse(Headers, "ConnectionIdB"))): _*)

      when(Inbound(ReserveConfirmed(Headers, "ConnectionIdA")))
      messages must beEmpty

      when(Inbound(ReserveConfirmed(Headers, "ConnectionIdB")))
      messages must contain(ReserveConfirmed(Headers.asReply, ConnectionId))

      connection.stateName must beEqualTo(ReservedReservationState)
    }

    "fail the reservation with a single path segment" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Inbound(ReserveResponse(Headers, "SegmentConnectionId"))): _*)

      when(Inbound(ReserveFailed(Headers, ConnectionId)))

      messages must contain(ReserveFailed(Headers.asReply, ConnectionId))
      connection.stateName must beEqualTo(FailedReservationState)
    }

    "fail the reservation with two segments and at least one fails" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A", "B"))),
        Inbound(ReserveResponse(Headers, "SegmentConnectionId")),
        Inbound(ReserveFailed(Headers, ConnectionId))): _*)

     when(Inbound(ReserveConfirmed(Headers, "connectionIdA")))

     messages must contain(ReserveFailed(Headers.asReply, ConnectionId))
     connection.stateName must beEqualTo(FailedReservationState)
    }.pendingUntilFixed

  }
}
