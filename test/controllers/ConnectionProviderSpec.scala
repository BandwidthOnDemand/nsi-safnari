package controllers

import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Promise
import nl.surfnet.safnari._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.specs2.matcher.BeNull
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.TypeValuePairListType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends org.specs2.mutable.Specification {

  def withEnvelope[T <: NsiMessage](message: T) = NsiEnvelope(NsiHeaders(message.correlationId, "RequesterNSA", "ProviderNSA", None), message)
  val InitialReserveType = new ReserveType().withCriteria(new ReservationRequestCriteriaType().
    withSchedule(new ScheduleType()).
    withBandwidth(100).
    withServiceAttributes(new TypeValuePairListType()).
    withPath(new PathType().withDirectionality(DirectionalityType.BIDIRECTIONAL)))
  val CorrelationId = newCorrelationId

  val Application = FakeApplication(additionalConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Reserve operation" should {
    "return the connection id and confirm the reservation" in new WithApplication(Application) {
      val requesterOperation: Promise[NsiRequesterOperation] = Promise()
      val response = await(ConnectionProvider.handleRequest(withEnvelope(Reserve(CorrelationId, InitialReserveType))) { requesterOperation.success(_) })

      response must beAnInstanceOf[ReserveResponse]
      response must beLike {
        case ReserveResponse(correlationId, connectionId) =>
          correlationId must beEqualTo(CorrelationId)
          connectionId must not(beEmpty)
      }

      await(requesterOperation.future) must beLike {
        case op: ReserveConfirmed =>
          op.correlationId must beEqualTo(CorrelationId)

          val queryResult = Promise[NsiRequesterOperation]
          await(ConnectionProvider.handleQuery(QuerySummary(newCorrelationId, Seq(op.connectionId))) { queryResult.success(_) })

          await(queryResult.future) must beLike {
            case QuerySummaryConfirmed(_, Seq(reservation: QuerySummaryResultType)) =>
              reservation.getConnectionId() must beEqualTo(op.connectionId)
          }
      }
    }
  }
}
