package functional
import play.api.Application
import java.net.{URI, URL}
import javax.xml.transform.dom.DOMResult
import jakarta.xml.ws.Holder

import controllers.NsiWebService
import nl.surfnet.nsiv2.soap.NsiSoapConversions._
import nl.surfnet.nsiv2.soap.ExtraBodyParsers
import nl.surfnet.nsiv2.messages._
import nl.surfnet.safnari._
import org.ogf.schemas.nsi._2013._12.connection.provider.ConnectionServiceProvider
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.headers._
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import org.w3c.dom.{Document, Element}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.test._
import play.api.routing.Router
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.routing.sird._

import scala.concurrent._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ReserveRequestSpec extends helpers.Specification {
  sequential

  val reserveConfirmed = Promise[NsiRequesterMessage[ReserveConfirmed]]

  val FakePceUri = s"http://localhost:$ServerPort"
  val FakeRequesterUri = s"http://localhost:$ServerPort/fake/requester"
  val FakeProviderUri = s"http://localhost:$ServerPort/fake/provider"
  val SafnariNsa = "urn:ogf:network:nsa:surfnet-nsi-safnari"
  val builder = new GuiceApplicationBuilder().configure(
    "nsi.actor" -> "real",
    "pce.actor" -> "real",
    "pce.endpoint" -> FakePceUri,
    "nsi.base.url" -> s"http://localhost:$ServerPort",
    "safnari.nsa.id" -> SafnariNsa,
    "nsi.twoway.tls" -> "false"
  )

  val application: Application = builder.appRoutes(app => {
      case ("POST", p"/fake/requester") => app.injector.instanceOf[ExtraBodyParsers].NsiRequesterEndPoint("fake-requester-nsa") {
        case message @ NsiRequesterMessage(headers, confirm: ReserveConfirmed) =>
          reserveConfirmed.success(NsiRequesterMessage(headers, confirm))
          Future.successful(message.ack())
        case response =>
          reserveConfirmed.failure(new RuntimeException(s"bad async response received: $response"))
          Future.successful(response.ack(ServiceException(new ServiceExceptionType().withNsaId("FAKE").withErrorId("FAKE").withText(s"$response"))))
      }
      case ("POST", p"/fake/provider") => app.injector.instanceOf[ExtraBodyParsers].NsiProviderEndPoint("fake-provider-nsa") {
        case message @ NsiProviderMessage(headers, reserve: InitialReserve) =>
          val connectionId = newConnectionId

          val requestCriteria = reserve.body.getCriteria
          val p2ps = requestCriteria.pointToPointService.get
          val confirmCriteria = requestCriteria.toInitialConfirmCriteria(p2ps.getSourceSTP, p2ps.getDestSTP).get

          headers.replyTo.foreach { replyTo =>
            app.injector.instanceOf[NsiWebService].callRequester(
              headers.requesterNSA,
              replyTo,
              message reply ReserveConfirmed(connectionId, confirmCriteria),
            new controllers.Configuration(builder.configuration))
          }
          Future.successful(message.ack(ReserveResponse(connectionId)))
        case wtf =>
          wtf.pp
          ???
      }
      case ("POST", p"/paths/find") =>
        val actionBuilder = app.injector.instanceOf[DefaultActionBuilder]
        val bodyParsers = app.injector.instanceOf[PlayBodyParsers]
        actionBuilder(bodyParsers.json) { request =>
          val pceRequest = Json.fromJson[PceRequest](request.body)
          pceRequest match {
            case JsSuccess(request: PathComputationRequest, _) =>
              val response: PceResponse = PathComputationConfirmed(request.correlationId, ComputedSegment(ProviderEndPoint("fake-provider-nsa", URI.create(FakeProviderUri)), request.serviceType) :: Nil)
              app.injector.instanceOf[WSClient].url(request.replyTo.toASCIIString()).post(Json.toJson(response))
              Results.Accepted
            case _ =>
              Results.BadRequest
          }
        }
    }
  ).build()

  def marshal(p2ps: P2PServiceBaseType): Element = {
    val jaxb = new org.ogf.schemas.nsi._2013._12.services.point2point.ObjectFactory().createP2Ps(p2ps)
    val result = new DOMResult()
    jaxbContext.createMarshaller().marshal(jaxb, result)
    result.getNode().asInstanceOf[Document].getDocumentElement();
  }

  "A reserve request" should {

    val NsiHeader = new Holder(new CommonHeaderType()
      .withCorrelationId("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def")
      .withProtocolVersion("2")
      .withRequesterNSA("fake-requester-nsa")
      .withProviderNSA(SafnariNsa)
      .withReplyTo(FakeRequesterUri))
    val ConnectionId = new Holder[String]()
    val Criteria = new ReservationRequestCriteriaType().
      withSchedule(new ScheduleType()).
      withServiceType("ServiceType").
      withAny(marshal(new P2PServiceBaseType().
        withCapacity(100).
        withDirectionality(DirectionalityType.BIDIRECTIONAL).
        withSourceSTP("networkId:source-localId").
        withDestSTP("networkId:dest-localId")))

    "send a reserve request to the ultimate provider agent" in new WithServer(application, ServerPort) {
      val service = new ConnectionServiceProvider(new URL(s"http://localhost:$port/nsi-v2/ConnectionServiceProvider"))

      service.getConnectionServiceProviderPort().reserve(ConnectionId, null, "description", Criteria, NsiHeader)

      await(reserveConfirmed.future).correlationId must beEqualTo(CorrelationId.fromString("urn:uuid:f8a23b90-832b-0130-d364-20c9d0879def").get)
    }

  }

}
