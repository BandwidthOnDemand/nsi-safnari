package nl.surfnet.safnari

import play.api.libs.json._
import java.net.URI
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.framework.types._
import play.api.data.validation.ValidationError
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType

object PceMessageSpec {
  val sourceStp = "network-id:source"

  val destStp = "network-id:dest"
  val ServiceBaseType = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP(sourceStp)
    .withDestSTP(destStp)
  val Schedule = new ScheduleType()
  val ServiceTypeUrl = "http://services.ogf.org/nsi/2013/07/descriptions/EVTS.A-GOLE"

  val correlationId = helpers.Specification.newCorrelationId

  val pathComputationRequest = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType(ServiceTypeUrl, ServiceBaseType), Nil)

  val providerEndPoint = ProviderEndPoint("provider-nsa", URI.create("http://localhost/pce/reply"), NoAuthentication)
  val computedSegment = ComputedSegment(providerEndPoint, ServiceType(ServiceTypeUrl, ServiceBaseType))
  val pathComputationResponse = PathComputationConfirmed(correlationId, Seq(computedSegment))

  val pathComputationFailedAck = PceFailed(correlationId, 404, "Not Accepted", "")
  val pathComputationAcceptedAck = PceAccepted(correlationId)
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PceMessageSpec extends helpers.Specification {
  import PceMessageSpec._

  "PceMessages" should {

    import nl.surfnet.safnari.PceMessage._

    "serialize request with p2pServiceBaseType to json" in {
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType(ServiceTypeUrl, ServiceBaseType), Nil)

      val json = Json.toJson(request)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))
      json \ "replyTo" \ "url" must beEqualTo(JsString("http://localhost/pce/reply"))
      (json \ "p.p2ps" apply 0) \ "capacity" must beEqualTo(JsNumber(100))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize request with connection trace to json" in {
      val first = new ConnectionType().withIndex(0).withValue("firstnsa")
      val second = new ConnectionType().withIndex(1).withValue("secondnsa")
      val connectionTrace = List(first, second)
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType(ServiceTypeUrl, ServiceBaseType), connectionTrace)

      val json = Json.toJson(request)
      (json \ "connectionTrace" apply 0) \ "index" must beEqualTo(JsNumber(first.getIndex))
      (json \ "connectionTrace" apply 1) \ "index" must beEqualTo(JsNumber(second.getIndex))
      (json \ "connectionTrace" apply 0) \ "value" must beEqualTo(JsString(first.getValue))
      (json \ "connectionTrace" apply 1) \ "value" must beEqualTo(JsString(second.getValue))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize computation failed response to json" in {
      val response = PathComputationFailed(correlationId, "FailedMessage")

      val json = Json.toJson[PceResponse](response)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))

      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "serialize computation confirmed response to json" in {
      val response = PathComputationConfirmed(correlationId, List(ComputedSegment(providerEndPoint, ServiceType(ServiceTypeUrl, ServiceBaseType))))

      val json = Json.toJson[PceResponse](response)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "deserialize path computation failed" in {
      val input = """{"correlationId":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED","message":"oops!"}"""

      val output = PathComputationFailed(CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get, "oops!")

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "toString of a pceResponse should not contain authorization information" in {
      BasicAuthentication("johndoe", "secret").toString must not(contain("secret"))
      OAuthAuthentication("secret-token").toString must not(contain("secret-token"))
    }

    "deserialize path computation confirmed" in {
      val input = """
        |{
        |  "correlationId": "urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02",
        |  "status": "SUCCESS",
        |  "path": [
        |    {
        |      "nsa": "urn:ogf:network:nsa:internet2.edu",
        |      "csProviderURL": "http://oscars.internet2.edu/provider",
        |      "credentials": {
        |        "method": "NONE"
        |      },
        |      "serviceType": "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
        |      "p.p2ps": [
        |        {
        |          "capacity": 100,
        |          "directionality": "Bidirectional",
        |          "symmetricPath": true,
        |          "sourceSTP": "urn:ogf:network:internet2.edu:i2-edge?vlan=1780",
        |          "destSTP": "urn:ogf:network:internet2.edu:to-esnet?vlan=1780"
        |        }
        |      ]
        |    },
        |    {
        |      "nsa": "urn:ogf:network:nsa:es.net",
        |      "csProviderURL": "http://oscars.es.net/nsi/ConnectionService",
        |      "credentials": {
        |        "method": "BASIC",
        |        "username": "foo",
        |        "password": "bar"
        |      },
        |      "serviceType": "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
        |      "p.p2ps": [
        |        {
        |          "capacity": 100,
        |          "directionality": "Bidirectional",
        |          "symmetricPath": true,
        |          "sourceSTP": "urn:ogf:network:es.net:to-internet2?vlan=1780",
        |          "destSTP": "urn:ogf:network:es.net:esnet-edge-one?vlan=1780"
        |        }
        |      ]
        |    }
        |  ]
        |}""".stripMargin

      val output = PathComputationConfirmed(
        CorrelationId.fromString("urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02").get,
        ComputedSegment(
          ProviderEndPoint("urn:ogf:network:nsa:internet2.edu",
            URI.create("http://oscars.internet2.edu/provider"),
            NoAuthentication),
          ServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE", new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSymmetricPath(true)
            .withSourceSTP("urn:ogf:network:internet2.edu:i2-edge?vlan=1780")
            .withDestSTP("urn:ogf:network:internet2.edu:to-esnet?vlan=1780")))
        :: ComputedSegment(
          ProviderEndPoint("urn:ogf:network:nsa:es.net",
            URI.create("http://oscars.es.net/nsi/ConnectionService"),
            BasicAuthentication("foo", "bar")),
          ServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE", new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSymmetricPath(true)
            .withSourceSTP("urn:ogf:network:es.net:to-internet2?vlan=1780")
            .withDestSTP("urn:ogf:network:es.net:esnet-edge-one?vlan=1780")))
        :: Nil)

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
      Json.fromJson[ProviderEndPoint](Json.parse("""{"nsa":"urn:nsa:surfnet.nl", "csProviderURL":"http://localhost", "credentials":{"method":"NONE"}}""")) must beEqualTo(
        JsSuccess(ProviderEndPoint("urn:nsa:surfnet.nl", URI.create("http://localhost"), NoAuthentication)))
    }

  }
}
