package nl.surfnet.safnari

import play.api.libs.json._
import java.net.URI
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types._
import play.api.data.validation.ValidationError

object PceMessageSpec {
  val sourceStp = new StpType().withNetworkId("network-id").withLocalId("source")

  val destStp = new StpType().withNetworkId("network-id").withLocalId("dest")

  val criteria = new ReservationConfirmCriteriaType().
    withSchedule(new ScheduleType()).withBandwidth(100).
    withPath(new PathType().withSourceSTP(sourceStp).withDestSTP(destStp))

  val correlationId = newCorrelationId

  val pathComputationRequest = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), criteria)

  val providerEndPoint = ProviderEndPoint("provider-nsa", URI.create("http://localhost/pce/reply"), NoAuthentication)
  val computedSegment = ComputedSegment(sourceStp, destStp, providerEndPoint)
  val pathComputationResponse = PathComputationConfirmed(correlationId, Seq(computedSegment))
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PceMessageSpec extends helpers.Specification {
  import PceMessageSpec._

  "PceMessages" should {

    import nl.surfnet.safnari.PceMessage._

    "serialize request to json" in {
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), criteria)

      val json = Json.toJson(request)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize computation failed response to json" in {
      val response = PathComputationFailed(correlationId, "FailedMessage")

      val json = Json.toJson[PceResponse](response)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "serialize computation confirmed response to json" in {
      val response = PathComputationConfirmed(correlationId, List(ComputedSegment(sourceStp, destStp, providerEndPoint)))

      val json = Json.toJson[PceResponse](response)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "deserialize stp type" in {
      val input = """{"network-id":"network-id","local-id":"local-id"}"""

      val stp = Json.fromJson[StpType](Json.parse(input))

      stp must beEqualTo(JsSuccess(new StpType().withNetworkId("network-id").withLocalId("local-id")))
    }

    "deserialize path computation failed" in {
      val input = """{"correlation-id":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED","message":"oops!"}"""

      val output = PathComputationFailed(CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get, "oops!")

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize path computation confirmed" in {
      val input = """{
        |"correlation-id":"urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02",
        |"status":"SUCCESS",
        |"message":"all good!",
        |"path":[{
        |"source-stp":{"network-id":"urn:ogf:network:stp:surfnet.nl","local-id":"15"},
        |"destination-stp":{"network-id":"urn:ogf:network:stp:surfnet.nl","local-id":"18"},
        |"nsa":"urn:ogf:network:nsa:surfnet.nl",
        |"provider-url":"http://localhost:8082/bod/v2/provider",
        |"auth":{"method":"OAUTH2","token":"f44b1e47-0a19-4c11-861b-c9abf82d4cbf"}}]
        |}""".stripMargin

      val output = PathComputationConfirmed(
        CorrelationId.fromString("urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02").get,
        ComputedSegment(
          new StpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("15"),
          new StpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("18"),
          ProviderEndPoint("urn:ogf:network:nsa:surfnet.nl",
          URI.create("http://localhost:8082/bod/v2/provider"),
          OAuthAuthentication("f44b1e47-0a19-4c11-861b-c9abf82d4cbf"))) :: Nil)

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize authentication method" in {
      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"foo"}""")) must beLike {
        case JsError(errors) =>
          errors must contain((JsPath \ "method") -> Seq(ValidationError("bad.authentication.method", "foo")))
      }

      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"BASIC"}""")) must beLike {
        case JsError(errors) =>
          errors must contain((JsPath \ "username") -> Seq(ValidationError("validate.error.missing-path")))
      }

      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"OAUTH2", "token":"oath2-token"}""")) must beEqualTo(JsSuccess(OAuthAuthentication("oath2-token")))

      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"BASIC", "username":"user", "password":"pwd"}""")) must beEqualTo(JsSuccess(BasicAuthentication("user", "pwd")))
    }

    "deserialize provider end point" in {
      Json.fromJson[ProviderEndPoint](Json.parse("""{"nsa":"urn:nsa:surfnet.nl", "provider-url":"http://localhost", "auth":{"method":"NONE"}}""")) must beEqualTo(
        JsSuccess(ProviderEndPoint("urn:nsa:surfnet.nl", URI.create("http://localhost"), NoAuthentication)))
    }

  }
}
