package controllers

import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Promise
import nl.surfnet.nsi._
import nl.surfnet.nsi.NsiProviderOperation._
import nl.surfnet.nsi.NsiRequesterOperation._
import nl.surfnet.nsi.NsiResponseMessage._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.specs2.matcher.BeNull
import org.ogf.schemas.nsi._2013._04.connection.types.ReserveType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends org.specs2.mutable.Specification {

  val CorrelationId = newCorrelationId

  "Reserve operation" should {
    "return the connection id and confirm the reservation" in new WithApplication {
      @volatile var requesterOperation: NsiRequesterOperation = null
      val response = Await.result(ConnectionProvider.handleRequest(Reserve(CorrelationId, new ReserveType)) { requesterOperation = _ }, Duration.Inf)

      response must beAnInstanceOf[ReserveResponse]
      response must beLike {
        case ReserveResponse(correlationId, connectionId) =>
          correlationId must beEqualTo(CorrelationId)
          connectionId must not(beEmpty)
      }

      requesterOperation must beAnInstanceOf[ReserveConfirmed]
      requesterOperation.correlationId must beEqualTo(CorrelationId)
    }
  }
}
