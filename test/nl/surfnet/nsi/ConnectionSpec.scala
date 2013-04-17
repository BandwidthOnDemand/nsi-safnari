package nl.surfnet.nsi

import java.util.UUID
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import java.net.URI

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends org.specs2.mutable.Specification {
  isolated

  val Headers = NsiHeaders(UUID.randomUUID, Some(URI.create("http://example.com/")))
  val ConnectionId = "ConnectionId"
  val CorrelationId = newCorrelationId
  val InitialMessages = Seq(Inbound(Reserve(Headers)), Outbound(ReserveResponse(Headers.copy(replyTo = None), ConnectionId)))

  var connection: Connection = NewConnection(ConnectionId)
  var messages: Seq[Message] = Nil

  def given(messages: Message*): Unit = {
    connection = messages.foldLeft(connection) { _.handle(_)._1 }
  }
  def when(message: Message): Unit = {
    val (c, m) = connection.handle(message)
    connection = c
    messages = m
  }

  "A connection" should {
    "send a reserve response when reserve is requested" in {
      when(Inbound(Reserve(Headers)))

      messages.head.asInstanceOf[ReserveResponse].headers must beEqualTo(Headers.copy(replyTo = None))
    }

    "send a path computation request when reserve is received" in {
      given(Inbound(Reserve(Headers)))

      when(Outbound(ReserveResponse(Headers, ConnectionId)))

      messages must haveOneElementLike { case request: PathComputationRequest => ok }
    }

    "fail the connection when path computation fails" in {
      given(InitialMessages :+ Outbound(PathComputationRequest(CorrelationId)): _*)

      when(Inbound(PathComputationFailed(CorrelationId)))

      messages must contain(ReserveFailed(Headers.copy(replyTo = None), ConnectionId))
    }

    "mark the reservation state failed" in {
      given(InitialMessages :+ Outbound(ReserveFailed(Headers.copy(replyTo = None), ConnectionId)): _*)

      connection.reservationState must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in {
      given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId, Seq("A"))),
        Outbound(Reserve(Headers)),
        Inbound(ReserveResponse(Headers, "SegmentConnectionId"))): _*)

      when(Inbound(ReserveConfirmed()))

      messages must contain(ReserveConfirmed())
    }

    "reserve two segments and be reserved" in {
      given(InitialMessages ++ Seq(
          Inbound(PathComputationConfirmed(CorrelationId, Seq("A", "B"))),
          Outbound(Reserve(Headers)),
          Outbound(Reserve(Headers)),
          Inbound(ReserveResponse(Headers, "ConnectionIdA")),
          Inbound(ReserveResponse(Headers, "ConnectionIdB"))): _*)

      when(Inbound(ReserveConfirmed()))
      messages must beEmpty

      when(Inbound(ReserveConfirmed()))
      messages must contain(ReserveConfirmed())

      when(Outbound(ReserveConfirmed()))

      connection.reservationState must beEqualTo(ReservedReservationState)
    }
  }
}
