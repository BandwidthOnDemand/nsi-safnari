package nl.surfnet.safnari

import org.ogf.schemas.nsi._2015._04.connection.pathtrace.{ PathTraceType, ObjectFactory => PathTraceTypeOF }
import nl.surfnet.nsiv2.messages._
import helpers.NsiMessages._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PathTraceSpec extends helpers.ConnectionEntitySpecification {
  "Connection" should {
    "send reserve request with added path trace element" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)))

      when(pce.confirm(CorrelationId(0, 1), A))

      messages must contain(like[Message] {
        case ToProvider(NsiProviderMessage(headers, _), provider) =>
          headers.pathTrace must beSome(new PathTraceType().withId(AggregatorNsa).withConnectionId(ConnectionId))
      })
    }

    "send reserve request with existing path trace element" in new fixture {
      val pathTrace = new PathTraceType().withId("RootAG").withConnectionId("RootAG-ConnectionId")
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType), Nil, new PathTraceTypeOF().createPathTrace(pathTrace) :: Nil))

      when(pce.confirm(CorrelationId(0, 1), A))

      messages must contain(like[Message] {
        case ToProvider(NsiProviderMessage(headers, _), provider) =>
          headers.pathTrace must beSome(pathTrace)
      })
    }
  }
}
