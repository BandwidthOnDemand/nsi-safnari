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
      val response = PathComputationFailed(correlationId)

      val json = Json.toJson(response)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "serialize computation confirmed response to json" in {
      val response = PathComputationConfirmed(correlationId, List(ComputedSegment(sourceStp, destStp, ProviderEndPoint("provider-nsa", URI.create("http://localhost/pce/reply"), NoAuthentication))))

      val json = Json.toJson(response)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "deserialize authentication method" in {
      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"foo"}""")) must beLike {
        case JsError(errors) =>
          errors must contain((JsPath \ "method") -> Seq(ValidationError("bad.authentication.method", "foo")))
      }
      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"basic"}""")) must beLike {
        case JsError(errors) =>
          errors must contain((JsPath \ "username") -> Seq(ValidationError("validate.error.missing-path")))
      }
      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"oauth2", "token":"oath2-token"}""")) must beEqualTo(JsSuccess(OAuthAuthentication("oath2-token")))
      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"basic", "username":"user", "password":"pwd"}""")) must beEqualTo(JsSuccess(BasicAuthentication("user", "pwd")))
    }
  }
}
