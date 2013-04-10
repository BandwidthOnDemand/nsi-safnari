package controllers

import play.api.test._
import models.NsiProviderOperation._
import models.NsiResponseMessage._
import models.NsiHeaders

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends org.specs2.mutable.Specification {

  "Reserve operation" should {
    "return the connection id" in {
      val response = ConnectionProvider.handleMessage(Reserve(NsiHeaders("correlationId", Some("http://localhost:19001/foo"))))

      response must beAnInstanceOf[ReserveResponse]
      response must beLike {
        case ReserveResponse(headers, connectionId) =>
          //headers must beEqualTo(NsiHeaders("correlationId", None))
          connectionId must not(beEmpty)
      }
    }
  }
}
