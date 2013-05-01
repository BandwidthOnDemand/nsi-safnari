package nl.surfnet.safnari

import play.api.libs.json._
import java.net.URI
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PceMessageSpec extends org.specs2.mutable.Specification {
  val sourceStp = new StpType().withNetworkId("network-id").withLocalId("source")

  val destStp = new StpType().withNetworkId("network-id").withLocalId("dest")

  val criteria = new ReservationConfirmCriteriaType().
    withSchedule(new ScheduleType()).withBandwidth(100).
    withPath(new PathType().withSourceSTP(sourceStp).withDestSTP(destStp))

  val correlationId = newCorrelationId

  "PceMessages" should {

    import nl.surfnet.safnari.PceMessage._

    "serialize request to json" in {
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), criteria)

      val json = Json.toJson(request)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PathComputationRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize computation failed response to json" in {
      val response = PathComputationFailed(correlationId)

      val json = Json.toJson(response)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "serialize computation confirmed response to json" in {
      val response = PathComputationConfirmed(correlationId, List(ComputedSegment(sourceStp, destStp, "provider-nsa", URI.create("http://localhost/pce/reply"), NoAuthentication)))

      val json = Json.toJson(response)

      json \ "correlation-id" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }
  }
}
