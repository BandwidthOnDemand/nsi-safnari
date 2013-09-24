package nl.surfnet.safnari

import play.api.libs.json._
import java.net.URI
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.framework.types._
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import play.api.data.validation.ValidationError
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._07.services.point2point.EthernetVlanType
import org.ogf.schemas.nsi._2013._07.services.types.DirectionalityType

object PceMessageSpec {
  val sourceStp = new StpType().withNetworkId("network-id").withLocalId("source")

  val destStp = new StpType().withNetworkId("network-id").withLocalId("dest")
  val EthernetVlanService = new EthernetVlanType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withSymmetricPath(true)
    .withCapacity(100)
    .withSourceSTP(sourceStp)
    .withDestSTP(destStp)
  val ServiceBaseType = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP(sourceStp)
    .withDestSTP(destStp)
  val Schedule = new ScheduleType()
  val ServiceType = "http://services.ogf.org/nsi/2013/07/descriptions/EVTS.A-GOLE"

  val correlationId = newCorrelationId

  val pathComputationRequest = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType, ServiceBaseType)

  val providerEndPoint = ProviderEndPoint("provider-nsa", URI.create("http://localhost/pce/reply"), NoAuthentication)
  val computedSegment = ComputedSegment(ServiceBaseType, providerEndPoint)
  val pathComputationResponse = PathComputationConfirmed(correlationId, Seq(computedSegment))
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PceMessageSpec extends helpers.Specification {
  import PceMessageSpec._

  "PceMessages" should {

    import nl.surfnet.safnari.PceMessage._

    "serialize request with ethernetVlanService to json" in {
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType, EthernetVlanService)

      val json = Json.toJson(request)

      println(json)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))
      json \ "replyTo" \ "url" must beEqualTo(JsString("http://localhost/pce/reply"))
      json \ "p:evts"
      println(json \ "p:evts")

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize request with p2pServiceBaseType to json" in {
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType, ServiceBaseType)

      val json = Json.toJson(request)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))
      json \ "replyTo" \ "url" must beEqualTo(JsString("http://localhost/pce/reply"))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize computation failed response to json" in {
      val response = PathComputationFailed(correlationId, "FailedMessage")

      val json = Json.toJson[PceResponse](response)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))

      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "serialize computation confirmed response to json" in {
      val response = PathComputationConfirmed(correlationId, List(ComputedSegment(ServiceBaseType, providerEndPoint)))

      val json = Json.toJson[PceResponse](response)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "deserialize stp type" in {
      val input = """{"networkId":"network-id","localId":"local-id"}"""

      val stp = Json.fromJson[StpType](Json.parse(input))

      stp must beEqualTo(JsSuccess(new StpType().withNetworkId("network-id").withLocalId("local-id")))
    }

    "deserialize path computation failed" in {
      val input = """{"correlationId":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED","message":"oops!"}"""

      val output = PathComputationFailed(CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get, "oops!")

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize path computation confirmed" in {
      val input = """{
        |"correlationId":"urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02",
        |"status":"SUCCESS",
        |"message":"all good!",
        |"path":[{
        |"capacity":100,
        |"sourceSTP":{"networkId":"urn:ogf:network:stp:surfnet.nl","localId":"15"},
        |"destSTP":{"networkId":"urn:ogf:network:stp:surfnet.nl","localId":"18"},
        |"nsa":"urn:ogf:network:nsa:surfnet.nl",
        |"providerUrl":"http://localhost:8082/bod/v2/provider",
        |"auth":{"method":"OAUTH2","token":"f44b1e47-0a19-4c11-861b-c9abf82d4cbf"}}]
        |}""".stripMargin

      val output = PathComputationConfirmed(
        CorrelationId.fromString("urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02").get,
        ComputedSegment(
          new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSourceSTP(new StpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("15"))
            .withDestSTP(new StpType().withNetworkId("urn:ogf:network:stp:surfnet.nl").withLocalId("18")),
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
          errors must contain((JsPath \ "username") -> Seq(ValidationError("error.path.missing")))
      }

      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"OAUTH2", "token":"oath2-token"}""")) must beEqualTo(JsSuccess(OAuthAuthentication("oath2-token")))

      Json.fromJson[ProviderAuthentication](Json.parse("""{"method":"BASIC", "username":"user", "password":"pwd"}""")) must beEqualTo(JsSuccess(BasicAuthentication("user", "pwd")))
    }

    "deserialize provider end point" in {
      Json.fromJson[ProviderEndPoint](Json.parse("""{"nsa":"urn:nsa:surfnet.nl", "providerUrl":"http://localhost", "auth":{"method":"NONE"}}""")) must beEqualTo(
        JsSuccess(ProviderEndPoint("urn:nsa:surfnet.nl", URI.create("http://localhost"), NoAuthentication)))
    }

  }
}
