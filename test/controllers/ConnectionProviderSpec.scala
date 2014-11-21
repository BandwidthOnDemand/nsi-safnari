package controllers

import akka.testkit.TestActorRef
import controllers.PathComputationEngine.DummyPceRequesterActor
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._12.connection.types._
import play.api.test._
import play.libs.Akka

import scala.concurrent.Promise

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionProviderSpec extends helpers.Specification {
  sequential

  import nl.surfnet.nsiv2.messages._
  import NsiMessageSpec._

  def Application = FakeApplication(additionalConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy"))

  "Reserve operation" should {

    "return the connection id and confirm the reservation" in new WithApplication(Application) {
      implicit val actorSystem = Akka.system
      val pceRequester = TestActorRef[DummyPceRequesterActor]
      val createOutboundActor = ConnectionProvider.outboundActor(ConnectionRequester.nsiRequester, pceRequester) _
      val connectionProvider = new ConnectionProvider(new ConnectionManager(ConnectionProvider.connectionFactory(createOutboundActor)))
      val requesterOperation = Promise[NsiRequesterOperation]()

      val response = await(connectionProvider.handleCommand(initialReserveMessage) { reply => requesterOperation.success(reply); () })

      response must beLike {
        case ReserveResponse(connectionId) => connectionId must not(beEmpty)
      }

      await(requesterOperation.future) must beLike {
        case op: ReserveConfirmed =>
          val queryResult = Promise[NsiRequesterOperation]

          await(connectionProvider.handleQuery(QuerySummary(Some(Left(Seq(op.connectionId)))), "RequesterNsa") { reply => queryResult.success(reply); () })

          await(queryResult.future) must beLike {
            case QuerySummaryConfirmed(Seq(reservation: QuerySummaryResultType)) =>
              reservation.getConnectionId() must beEqualTo(op.connectionId)
          }
      }
    }

  }
}
