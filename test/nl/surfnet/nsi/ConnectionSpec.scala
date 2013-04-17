package nl.surfnet.nsi

import java.util.UUID
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import java.net.URI

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends org.specs2.mutable.Specification {

  val Headers = NsiHeaders(UUID.randomUUID, Some(URI.create("http://example.com/")))
  val ConnectionId = "ConnectionId"
  val CorrelationId = newCorrelationId
  val InitialMessages = Seq(Inbound(Reserve(Headers)), Outbound(ReserveResponse(Headers.copy(replyTo = None), ConnectionId)))
  val InitialConnection = given(InitialMessages: _*)

  def given(messages: Message*): Connection = messages.foldLeft(NewConnection(ConnectionId): Connection) { _.handle(_)._1 }
  def when(connection: Connection, message: Message): Seq[Message] = connection.handle(message)._2

  "A connection" should {
    "send a reserve response when reserve is requested" in {
      val connection = given()

      val Seq(response: ReserveResponse) = when(connection, Inbound(Reserve(Headers)))

      response.headers must beEqualTo(Headers.copy(replyTo = None))
    }

    "send a path computation request when reserve is received" in {
      val connection = given(Inbound(Reserve(Headers)))

      val outbound = when(connection, Outbound(ReserveResponse(Headers, ConnectionId)))

      outbound must haveOneElementLike { case request: PathComputationRequest => ok }
    }

    "fail the connection when path computation fails" in {
      val connection = given(InitialMessages :+ Outbound(PathComputationRequest(CorrelationId)): _*)

      val outbound = when(connection, Inbound(PathComputationFailed(CorrelationId)))

      outbound must contain(ReserveFailed(Headers.copy(replyTo = None), ConnectionId))
    }

    "mark the reservation state failed" in {
      val connection = given(InitialMessages :+ Outbound(ReserveFailed(Headers.copy(replyTo = None), ConnectionId)): _*)

      connection.reservationState must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in {
      val connection = given(InitialMessages ++ Seq(
        Inbound(PathComputationConfirmed(CorrelationId)),
        Outbound(Reserve(Headers)),
        Inbound(ReserveResponse(Headers, "SegmentConnectionId"))): _*)

      val outbound = when(connection, Inbound(ReserveConfirmed()))

      outbound must contain(ReserveConfirmed())
    }
  }
}
