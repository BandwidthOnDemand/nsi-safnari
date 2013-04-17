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
  val InitialConnection = NewConnection(ConnectionId).handle(Inbound(Reserve(Headers)))._1.handle(Outbound(ReserveResponse(Headers.copy(replyTo = None), ConnectionId)))

  "A connection" should {
    "send a reserve response when reserve is requested" in {
      val Seq(response: ReserveResponse) = NewConnection(ConnectionId).handle(Inbound(Reserve(Headers)))._2

      response.headers must beEqualTo(Headers.copy(replyTo = None))
    }

    "send a path computation request when reserve is received" in {
      val connection = NewConnection(ConnectionId).handle(Inbound(Reserve(Headers)))._1

      val outbound = connection.handle(Outbound(ReserveResponse(Headers, ConnectionId)))._2

      outbound must haveOneElementLike { case request: PathComputationRequest => ok }
    }

    "fail the connection when path computation fails" in {
      val connection = InitialConnection._1.handle(Outbound(PathComputationRequest(CorrelationId)))._1

      val outbound = connection.handle(Inbound(PathComputationFailed(CorrelationId)))._2

      outbound must contain(ReserveFailed(Headers.copy(replyTo = None), ConnectionId))
    }

    "mark the reservation state failed" in {
      val connection = InitialConnection._1.handle(Outbound(ReserveFailed(Headers.copy(replyTo = None), ConnectionId)))._1

      connection.reservationState must beEqualTo(FailedReservationState)
    }

    "confirm the reservation with a single path segment" in {
      val connection = InitialConnection._1.handle(Inbound(PathComputationConfirmed(CorrelationId)))._1.
        handle(Outbound(Reserve(Headers)))._1.
        handle(Inbound(ReserveResponse(Headers, "SegmentConnectionId")))._1

      val outbound = connection.handle(Inbound(ReserveConfirmed()))._2

      outbound must contain(ReserveConfirmed())
    }
  }
}
