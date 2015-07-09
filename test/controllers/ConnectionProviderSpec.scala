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

  val DefaultConfiguration = Map("nsi.actor" -> "dummy", "pce.actor" -> "dummy")

  def Application(extraConfig: (String, String)*) = FakeApplication(additionalConfiguration = DefaultConfiguration ++ extraConfig)

  abstract class Fixture(app: FakeApplication) extends WithApplication(app) {
    implicit lazy val actorSystem = Akka.system
    lazy val pceRequester = TestActorRef[DummyPceRequesterActor]
    lazy val createOutboundActor = ConnectionProvider.outboundActor(ConnectionRequester.nsiRequester, pceRequester) _
    lazy val connectionProvider = new ConnectionProvider(new ConnectionManager(ConnectionProvider.connectionFactory(createOutboundActor)))
    lazy val requesterOperation = Promise[NsiRequesterOperation]()
  }

  "Reserve operation" should {

    "return the connection id and confirm the reservation" in new Fixture(Application()) {
      val response = await(connectionProvider.handleCommand(initialReserveMessage) { reply => requesterOperation.success(reply.body); () })

      response must beLike {
        case ReserveResponse(connectionId) => connectionId must not(beEmpty)
      }

      await(requesterOperation.future) must beLike {
        case op: ReserveConfirmed =>
          val queryResult = Promise[NsiRequesterOperation]

          await(connectionProvider.handleQuery(NsiProviderMessage(nsiRequesterHeaders(CorrelationId(0, 3)), QuerySummary(Some(Left(Seq(op.connectionId)))))) { reply => queryResult.success(reply.body); () })

          await(queryResult.future) must beLike {
            case QuerySummaryConfirmed(Seq(reservation: QuerySummaryResultType)) =>
              reservation.getConnectionId() must beEqualTo(op.connectionId)
          }
      }
    }

  }

  "Any operation" should {
    "check requester NSA against TLS configuration" in new Fixture(Application("nsi.twoway.tls" -> "yes")) {
      val response = connectionProvider.request.apply(FakeRequest().withBody(initialReserveMessage))

      val body = scala.xml.XML.loadString(contentAsString(response))
      body must \\("text") \> "Parameter provided contains an unsupported value which MUST be processed"
      body must \\("variable", "type" -> "requesterNSA")
      body must \\("variable") \("value") \> RequesterNsa
    }
  }
}
