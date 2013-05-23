package controllers

import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Promise
import nl.surfnet.safnari._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.TypeValuePairListType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends helpers.Specification {
  import nl.surfnet.safnari.NsiMessageSpec._

  def Application = FakeApplication(additionalConfiguration = testConfiguration ++ Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Reserve operation" should {

    "return the connection id and confirm the reservation" in new WithApplication(Application) {
      val requesterOperation: Promise[NsiRequesterOperation] = Promise()
      val response = await(ConnectionProvider.handleRequest(initialReserveMessage) { requesterOperation.success(_) })

      response must beAnInstanceOf[ReserveResponse]
      response must beLike {
        case ReserveResponse(headers, connectionId) =>
          headers.correlationId must beEqualTo(CorrelationId)
          connectionId must not(beEmpty)
      }

      await(requesterOperation.future) must beLike {
        case op: ReserveConfirmed =>
          op.correlationId must beEqualTo(CorrelationId)

          val queryResult = Promise[NsiRequesterOperation]
          await(ConnectionProvider.handleQuery(QuerySummary(headers(newCorrelationId), Seq(op.connectionId))) { queryResult.success(_) })

          await(queryResult.future) must beLike {
            case QuerySummaryConfirmed(_, Seq(reservation: QuerySummaryResultType)) =>
              reservation.getConnectionId() must beEqualTo(op.connectionId)
          }
      }
    }

  }
}
