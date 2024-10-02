package nl.surfnet.safnari

import helpers.NsiMessages._
import nl.surfnet.nsiv2.messages._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PathTraceSpec extends helpers.ConnectionEntitySpecification {

  "Root aggregator" should {
    "add path trace element to reserve request" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

      when(pce.confirm(CorrelationId(0, 1), A))

      messages must contain(like[Message] { case ToProvider(NsiProviderMessage(headers, _), _) =>
        headers.pathTrace must beSome(emptyPathTrace(AggregatorNsa, ConnectionId).getValue())
      })
    }

    "add aggregated path trace element in reserve confirmed reply with single segment" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), any = Nil),
        pce.confirm(CorrelationId(0, 1), A),
        upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA"))
      )

      when(
        upa.response(
          CorrelationId(0, 4),
          ReserveConfirmed("ConnectionIdA", ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            (A.provider.nsa, "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )

      messages must contain(
        agg.response(
          ReserveCorrelationId,
          ReserveConfirmed(ConnectionId, ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            (A.provider.nsa, "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )
    }

    "add aggregated path trace element in reserve confirmed reply with multiple segments" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), any = Nil),
        pce.confirm(CorrelationId(0, 1), A, B),
        upa.response(
          CorrelationId(0, 5),
          ReserveConfirmed("ConnectionIdB", ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            ("ChildB", "ConnectionIdB") -> List("stp-3", "stp-4")
          ) :: Nil
        )
      )

      when(
        upa.response(
          CorrelationId(0, 4),
          ReserveConfirmed("ConnectionIdA", ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )

      messages must contain(
        agg.response(
          ReserveCorrelationId,
          ReserveConfirmed(ConnectionId, ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2"),
            ("ChildB", "ConnectionIdB") -> List("stp-3", "stp-4")
          ) :: Nil
        )
      )
    }

    "add aggregated path trace element in reserve commit request" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), any = Nil),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(
          CorrelationId(0, 4),
          ReserveConfirmed("ConnectionIdA", ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )

      when(ura.request(CorrelationId(0, 2), ReserveCommit(ConnectionId)))

      messages must contain(
        agg.request(
          CorrelationId(0, 6),
          ReserveCommit("ConnectionIdA"),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )
    }
  }

  "Intermediate aggregator connection" should {
    "send reserve request with existing path trace element" in new fixture {
      val pathTraceHeader = emptyPathTrace("RootAG", "RootAG-ConnectionId")
      given(
        ura.request(
          ReserveCorrelationId,
          InitialReserve(InitialReserveType),
          any = pathTraceHeader :: Nil
        )
      )

      when(pce.confirm(CorrelationId(0, 1), A))

      messages must contain(like[Message] { case ToProvider(NsiProviderMessage(headers, _), _) =>
        headers.pathTrace must beSome(pathTraceHeader.getValue())
      })
    }

    "send aggregated path trace element in reserve confirmed reply" in new fixture {
      val pathTraceHeader = emptyPathTrace("RootAG", "RootAG-ConnectionId")
      given(
        ura.request(
          ReserveCorrelationId,
          InitialReserve(InitialReserveType),
          any = pathTraceHeader :: Nil
        ),
        pce.confirm(CorrelationId(0, 1), A),
        upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA"))
      )

      when(
        upa.response(
          CorrelationId(0, 4),
          ReserveConfirmed("ConnectionIdA", ConfirmCriteria),
          any = pathTrace(
            AggregatorNsa,
            ConnectionId,
            ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )

      messages must contain(
        agg.response(
          ReserveCorrelationId,
          ReserveConfirmed(ConnectionId, ConfirmCriteria),
          any = pathTrace(
            "RootAG",
            "RootAG-ConnectionId",
            ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2")
          ) :: Nil
        )
      )
    }

    "pass aggregated path trace element in reserve commit" in new fixture {
      val initialReservePathTrace = emptyPathTrace("RootAG", "RootAG-ConnectionId")
      val completedPathTrace = pathTrace(
        "RootAG",
        "RootAG-ConnectionId",
        ("ChildA", "ConnectionIdA") -> List("stp-1", "stp-2")
      )
      given(
        ura.request(
          ReserveCorrelationId,
          InitialReserve(InitialReserveType),
          any = initialReservePathTrace :: Nil
        ),
        pce.confirm(CorrelationId(0, 1), A),
        upa.response(
          CorrelationId(0, 4),
          ReserveConfirmed("ConnectionIdA", ConfirmCriteria),
          any = completedPathTrace :: Nil
        )
      )

      when(
        ura.request(
          CorrelationId(0, 2),
          ReserveCommit(ConnectionId),
          any = completedPathTrace :: Nil
        )
      )

      messages must contain(
        agg.request(
          CorrelationId(0, 6),
          ReserveCommit("ConnectionIdA"),
          any = completedPathTrace :: Nil
        )
      )
    }
  }
}
