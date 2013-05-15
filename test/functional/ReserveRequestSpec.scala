package functional

import java.net.URL
import java.util.Collections
import javax.xml.ws.Holder
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
import play.api.libs.json._
import java.net.URI
import com.twitter.bijection.Injection
import support.ExtraBodyParsers._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ReserveRequestSpec extends helpers.Specification {

  val reserveConfirmed = Promise[ReserveConfirmed]

  object Global extends play.api.GlobalSettings {
    override def onRouteRequest(request: RequestHeader): Option[Handler] = request.path match {
      case "/fake/requester" => Some(NsiRequesterEndPoint {
        case NsiEnvelope(headers, confirm: ReserveConfirmed) =>
          reserveConfirmed.success(confirm)
          Future.successful(GenericAck(headers.correlationId))
        case response =>
          reserveConfirmed.failure(new RuntimeException(s"bad async response received: $response"))
          Future.successful(ServiceException(response.headers.correlationId, new ServiceExceptionType().withNsaId("FAKE").withErrorId("FAKE").withText(s"$response")))
      })
      case "/fake/provider" => Some(NsiProviderEndPoint {
        case NsiEnvelope(headers, reserve: Reserve) =>
          val connectionId = newConnectionId
          headers.replyTo.foreach { replyTo =>
            WS.url(replyTo.toASCIIString()).post(NsiEnvelope(headers.asReply, ReserveConfirmed(headers.correlationId, connectionId, Injection.invert(reserve.body.getCriteria()).get)))
          }
          Future.successful(ReserveResponse(headers.correlationId, connectionId))
      })
      case "/fake/pce" =>
        Some(Action(BodyParsers.parse.json) { request =>
          val pceRequest = Json.fromJson[PathComputationRequest](request.body)
          pceRequest match {
            case JsSuccess(request, _) =>
              val response = PathComputationConfirmed(request.correlationId, ComputedSegment(request.criteria.getPath().getSourceSTP(), request.criteria.getPath().getDestSTP(), ProviderEndPoint("provider-nsa", URI.create(FakeProviderUri), NoAuthentication)) :: Nil)
              WS.url(request.replyTo.toASCIIString()).post(Json.toJson(response))
              Results.Ok
            case _ =>
              Results.BadRequest
          }
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
    "pce.actor" -> "real",
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

      service.getConnectionServiceProviderPort().reserve(ConnectionId, null, "description", Criteria, NsiHeader)

      await(reserveConfirmed.future).correlationId must beEqualTo(CorrelationId.fromString("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def").get)
    }

  }

}
