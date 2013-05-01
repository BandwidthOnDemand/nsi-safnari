package functional

import java.net.URL
import java.util.Collections
import javax.xml.ws.Holder
import org.junit.runner.RunWith
import org.specs2.execute.PendingUntilFixed
import org.specs2.mutable.Specification
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.headers._
import org.ogf.schemas.nsi._2013._04.framework.types._
import org.ogf.schemas.nsi._2013._04.connection.provider.ConnectionServiceProvider
import support.ExtraBodyParsers
import scala.concurrent._
import nl.surfnet.safnari._
import play.api.libs.concurrent.Execution
import play.api.libs.ws.WS

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ReserveRequestSpec extends Specification with PendingUntilFixed {

  val reserveConfirmed = Promise[ReserveConfirmed]

  object Global extends play.api.GlobalSettings {
    override def onRouteRequest(request: RequestHeader): Option[Handler] = request.path match {
      case "/fake/requester" => Some(ExtraBodyParsers.NsiRequesterEndPoint {
        case NsiEnvelope(headers, confirm: ReserveConfirmed) =>
          reserveConfirmed.success(confirm)
          Future.successful(GenericAck(headers.correlationId))
        case response =>
          reserveConfirmed.failure(new RuntimeException(s"bad async response received: $response"))
          Future.successful(ServiceException(response.headers.correlationId, s"$response"))
      })
      case "/fake/provider" => println("message for fake provider"); ???
      case "/fake/pce" =>
        Some(Action(BodyParsers.parse.json) { request =>
          request.body.pp;
          WS.url((request.body \ "reply-to").as[String] + "/" + (request.body \ "correlation-id").as[String]).post(request.body).onSuccess {
            case request => request.status.pp("pce notification result")
          }(Execution.defaultContext)
          Results.Ok
        })
      case _ => super.onRouteRequest(request)
    }
  }

  val ServerPort = Helpers.testServerPort
  val FakePceUri = s"http://localhost:$ServerPort/fake/pce"
  val FakeRequesterUri = s"http://localhost:$ServerPort/fake/requester"
  val FakeProviderUri = s"http://localhost:$ServerPort/fake/provider"
  val Application = FakeApplication(additionalConfiguration = Map(
    "nsi.actor" -> "real",
    "pce.actor" -> "dummy",
    "pce.endpoint" -> FakePceUri,
    "nsi.base.url" -> s"http://localhost:$ServerPort"), withGlobal = Some(Global))

  "A reserve request" should {

    val NsiHeader = new Holder(new CommonHeaderType()
      .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
      .withProtocolVersion("2")
      .withRequesterNSA("urn:ogf:network:surfnet")
      .withProviderNSA("urn:ogf:network:safnari")
      .withReplyTo(FakeRequesterUri))
    val ConnectionId = new Holder[String]()
    val Criteria = new ReservationRequestCriteriaType().
      withSchedule(new ScheduleType()).
      withBandwidth(100).
      withServiceAttributes(new TypeValuePairListType()).
      withPath(new PathType().withDirectionality(DirectionalityType.BIDIRECTIONAL).withSourceSTP(new StpType().withNetworkId("networkId").withLocalId("source-localId")).withDestSTP(new StpType().withNetworkId("networkId").withLocalId("dest-localId")))

    "send a reserve request to the ultimate provider agent" in new WithServer(Application, ServerPort) {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      service.getConnectionServiceProviderPort().reserve(null, "description", ConnectionId, Criteria, NsiHeader)

      await(reserveConfirmed.future).correlationId must beEqualTo("f8a23b90-832b-0130-d364-20c9d0879def")
    }.pendingUntilFixed

  }

}