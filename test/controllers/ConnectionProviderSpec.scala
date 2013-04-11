package controllers

import play.api.test._
import models.NsiProviderOperation._
import models.NsiResponseMessage._
import models.NsiHeaders
import java.util.UUID

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends org.specs2.mutable.Specification {

  val CorrelationId = UUID.randomUUID

  "Reserve operation" should {
    "return the connection id" in {
      val response = ConnectionProvider.handleMessage(Reserve(NsiHeaders(CorrelationId, None)))

      response must beAnInstanceOf[ReserveResponse]
      response must beLike {
        case ReserveResponse(headers, connectionId) =>
          headers must beEqualTo(NsiHeaders(CorrelationId, None))
          connectionId must not(beEmpty)
      }
    }
  }
}
