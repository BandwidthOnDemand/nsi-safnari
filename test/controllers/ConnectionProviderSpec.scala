package controllers

import play.api.test._
import play.api.test.Helpers._
import java.util.UUID
import scala.concurrent.Promise
import nl.surfnet.nsi._
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.specs2.matcher.BeNull

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends org.specs2.mutable.Specification {

  val CorrelationId = UUID.randomUUID

  "Reserve operation" should {

    "return the connection id" in {
      var requesterOperation: NsiRequesterOperation = null
      val response = Await.result(ConnectionProvider.handleRequest(Reserve(NsiHeaders(CorrelationId, None))) { requesterOperation = _ }, Duration.Inf)

      response must beAnInstanceOf[ReserveResponse]
      response must beLike {
        case ReserveResponse(headers, connectionId) =>
          headers must beEqualTo(NsiHeaders(CorrelationId, None))
          connectionId must not(beEmpty)
      }

      //ConnectionProvider.handleResponse(PathComputationFailed(ConnectionProvider.continuations.single.head._1))

      // Send fake failed route response

      requesterOperation must not(beNull)
      requesterOperation.correlationId must beEqualTo(CorrelationId)
    }
  }
}
