package nl.surfnet.nsi

import java.util.UUID
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiAcknowledgement._
import java.net.URI

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends org.specs2.mutable.Specification {

  val Headers = NsiHeaders(UUID.randomUUID, Some(URI.create("http://example.com/")))
  val ConnectionId = "ConnectionId"
  val CorrelationId = newCorrelationId
  val InitialConnection = NewConnection(ConnectionId).handleInbound(Reserve(Headers))._1.handleAck(ReserveResponse(Headers.copy(replyTo = None), ConnectionId))

  "A connection" should {
    "send a reserve ack when reserve is requested" in {
      val ack = NewConnection(ConnectionId).handleInbound(Reserve(Headers))._2

      ack.asInstanceOf[NsiAcknowledgement].headers must beEqualTo(Headers.copy(replyTo = None))
    }

    "send a path computation request when reserve is received" in {
      val connection = NewConnection(ConnectionId).handleInbound(Reserve(Headers))._1

      val outbound = connection.handleAck(ReserveResponse(Headers, ConnectionId))._2

      outbound must haveOneElementLike { case request: PathComputationRequest => ok }
    }

    "fail the connection when path computation fails" in {
      val connection = InitialConnection._1.handleOutbound(PathComputationRequest(CorrelationId))._1

      val outbound = connection.handleInbound(PathComputationFailed(CorrelationId))._3

      outbound must contain(ReserveFailed(Headers.copy(replyTo = None), ConnectionId))
    }

    "mark the reservation state failed" in {
      val connection = InitialConnection._1.handleOutbound(ReserveFailed(Headers.copy(replyTo = None), ConnectionId))._1

      connection.reservationState must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in {
      val connection = InitialConnection._1.handleInbound(PathComputationConfirmed(CorrelationId))._1.
        handleOutbound(Reserve(Headers))._1.
        handleAck(ReserveResponse(Headers, "SegmentConnectionId"))._1

      val outbound = connection.handleInbound(ReserveConfirmed(Headers))._3

      outbound must contain(ReserveConfirmed(Headers.copy(replyTo = None)))
    }
  }
}
