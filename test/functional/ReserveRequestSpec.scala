package functional

import java.net.URL
import java.util.Collections
import javax.xml.ws.Holder
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.headers._
import org.ogf.schemas.nsi._2013._07.framework.types._
import org.ogf.schemas.nsi._2013._07.connection.provider.ConnectionServiceProvider
import support.ExtraBodyParsers
import scala.concurrent._
import nl.surfnet.safnari._
import nl.surfnet.safnari.NsiSoapConversions._
import play.api.libs.concurrent.Execution
import play.api.libs.ws.WS
import play.api.libs.json._
import java.net.URI
import support.ExtraBodyParsers._
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.types.DirectionalityType
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import javax.xml.bind.JAXBElement
import org.w3c.dom.Element
import javax.xml.transform.dom.DOMResult
import org.w3c.dom.Document

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ReserveRequestSpec extends helpers.Specification {

  val reserveConfirmed = Promise[ReserveConfirmed]

  object Global extends play.api.GlobalSettings {
    override def onRouteRequest(request: RequestHeader): Option[Handler] = request.path match {
      case "/fake/requester" => Some(NsiRequesterEndPoint {
        case confirm: ReserveConfirmed =>
          reserveConfirmed.success(confirm)
          Future.successful(GenericAck(confirm.headers.asReply))
        case response =>
          reserveConfirmed.failure(new RuntimeException(s"bad async response received: $response"))
          Future.successful(ServiceException(response.headers.asReply, new ServiceExceptionType().withNsaId("FAKE").withErrorId("FAKE").withText(s"$response")))
      })
      case "/fake/provider" => Some(NsiProviderEndPoint {
        case reserve: InitialReserve =>
          val connectionId = newConnectionId
          reserve.headers.replyTo.foreach { replyTo =>
            WS.url(replyTo.toASCIIString()).post(ReserveConfirmed(reserve.headers.asReply, connectionId, Conversion.invert(reserve.body.getCriteria()).right.get): NsiRequesterOperation)
          }
          Future.successful(ReserveResponse(reserve.headers.asReply, connectionId))
        case wtf =>
          wtf.pp
          ???
      })
      case "/fake/pce" =>
        Some(Action(BodyParsers.parse.json) { request =>
          val pceRequest = Json.fromJson[PceRequest](request.body)
          pceRequest match {
            case JsSuccess(request: PathComputationRequest, _) =>
              val response = PathComputationConfirmed(request.correlationId, ComputedSegment(request.service, ProviderEndPoint("provider-nsa", URI.create(FakeProviderUri), NoAuthentication)) :: Nil)
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
  val Application = FakeApplication(additionalConfiguration = testConfiguration ++ Map(
    "nsi.actor" -> "real",
    "pce.actor" -> "real",
    "pce.endpoint" -> FakePceUri,
    "nsi.base.url" -> s"http://localhost:$ServerPort"), withGlobal = Some(Global))

  def marshal(p2ps: P2PServiceBaseType): Element = {
    val jaxb = new org.ogf.schemas.nsi._2013._07.services.point2point.ObjectFactory().createP2Ps(p2ps)
    val result = new DOMResult()
    jaxbContext.createMarshaller().marshal(jaxb, result)
    result.getNode().asInstanceOf[Document].getDocumentElement();
  }

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
      withAny(marshal(new P2PServiceBaseType().
          withCapacity(100).
          withDirectionality(DirectionalityType.BIDIRECTIONAL).
          withSourceSTP(new StpType().withNetworkId("networkId").withLocalId("source-localId")).
          withDestSTP(new StpType().withNetworkId("networkId").withLocalId("dest-localId"))))

    "send a reserve request to the ultimate provider agent" in new WithServer(Application, ServerPort) {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      service.getConnectionServiceProviderPort().reserve(ConnectionId, null, "description", Criteria, NsiHeader)

      await(reserveConfirmed.future).correlationId must beEqualTo(CorrelationId.fromString("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def").get)
    }

  }

}
