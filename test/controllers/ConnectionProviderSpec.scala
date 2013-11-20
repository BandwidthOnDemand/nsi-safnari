package controllers

import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Promise
import nl.surfnet.safnari._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types.TypeValuePairListType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends helpers.Specification {
  sequential

  import nl.surfnet.safnari.NsiMessageSpec._

  def Application = FakeApplication(additionalConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Reserve operation" should {

    "return the connection id and confirm the reservation" in new WithApplication(Application) {
      val requesterOperation: Promise[NsiRequesterOperation] = Promise()
      val response = await(ConnectionProvider.handleCommand(initialReserveMessage) { requesterOperation.success(_) })

      response must beLike {
        case ReserveResponse(connectionId) =>
          connectionId must not(beEmpty)
      }

      await(requesterOperation.future) must beLike {
        case op: ReserveConfirmed =>
          val queryResult = Promise[NsiRequesterOperation]

          await(ConnectionProvider.handleQuery(QuerySummary(Some(Left(Seq(op.connectionId)))), "RequesterNsa") { queryResult.success(_) })

          await(queryResult.future) must beLike {
            case QuerySummaryConfirmed(Seq(reservation: QuerySummaryResultType)) =>
              reservation.getConnectionId() must beEqualTo(op.connectionId)
          }
      }
    }

  }
}
