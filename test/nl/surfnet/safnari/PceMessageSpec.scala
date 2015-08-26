package nl.surfnet.safnari

import java.net.URI
import java.util.Collection

import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import nl.surfnet.nsiv2.messages.CorrelationId
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.connection.types._
import org.ogf.schemas.nsi._2013._12.services.types._
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import play.api.libs.json._

import nl.surfnet.nsiv2.messages._

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

  val pathComputationRequest = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType(ServiceTypeUrl, ServiceBaseType), ChainAlgorithm, Nil)

  val providerEndPoint = ProviderEndPoint("provider-nsa", URI.create("http://localhost/pce/reply"))
  val computedSegment = ComputedSegment(providerEndPoint, ServiceType(ServiceTypeUrl, ServiceBaseType))
  val pathComputationResponse = PathComputationConfirmed(correlationId, Seq(computedSegment))

  val pathComputationFailedAck = PceFailed(correlationId, 404, "Not Accepted", "")
  val pathComputationAcceptedAck = PceAccepted(correlationId)
}

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PceMessageSpec extends helpers.Specification {
  import nl.surfnet.safnari.PceMessageSpec._

  "PceMessages" should {

    import nl.surfnet.safnari.PceMessage._

    "serialize request with p2pServiceBaseType to json" in {
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType(ServiceTypeUrl, ServiceBaseType), ChainAlgorithm, Nil)

      val json = Json.toJson(request)

      json \ "correlationId" must beEqualTo(JsString(correlationId.toString))
      json \ "replyTo" \ "url" must beEqualTo(JsString("http://localhost/pce/reply"))
      json \ "algorithm" must beEqualTo(JsString("CHAIN"))
      (json \ "p.p2ps" apply 0) \ "capacity" must beEqualTo(JsNumber(100))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize request with connection trace to json" in {
      val first = new ConnectionType().withIndex(0).withValue("firstnsa")
      val second = new ConnectionType().withIndex(1).withValue("secondnsa")
      val connectionTrace = List(first, second)
      val request = PathComputationRequest(correlationId, URI.create("http://localhost/pce/reply"), Schedule, ServiceType(ServiceTypeUrl, ServiceBaseType), ChainAlgorithm, connectionTrace)

      val json = Json.toJson(request)
      (json \ "trace" apply 0) \ "index" must beEqualTo(JsNumber(first.getIndex))
      (json \ "trace" apply 1) \ "index" must beEqualTo(JsNumber(second.getIndex))
      (json \ "trace" apply 0) \ "value" must beEqualTo(JsString(first.getValue))
      (json \ "trace" apply 1) \ "value" must beEqualTo(JsString(second.getValue))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize computation failed response to json" in {
      val response = PathComputationFailed(correlationId, NsiError.NoPathFound)

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
      val input =
        """{"correlationId":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED",
            "m.findPathError":{"code":"00702","label":"LABEL","description":"oops!","variable":{"@type":"type", "value":"value"}}}""".stripMargin

      val output = PathComputationFailed(CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get, NsiError("00702", "LABEL", "oops!", Some(NsiErrorVariable("type", "value"))))

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize path computation failed from version 1 message" in {
      val input =
        """{"correlationId":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED", "message": "oops!"}""".stripMargin

      val output = PathComputationFailed(CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get, NsiError.TopologyError.copy(text = "oops!"))

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
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
        |      "serviceType": "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
        |      "p.p2ps": [
        |        {
        |          "capacity": 100,
        |          "directionality": "Bidirectional",
        |          "symmetricPath": true,
        |          "sourceSTP": "urn:ogf:network:internet2.edu:i2-edge?vlan=1780",
        |          "destSTP": "urn:ogf:network:internet2.edu:to-esnet?vlan=1780",
        |          "parameter":[]
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
        |          "destSTP": "urn:ogf:network:es.net:esnet-edge-one?vlan=1780",
        |          "parameter":[]
        |        }
        |      ]
        |    }
        |  ]
        |}""".stripMargin

      val output = PathComputationConfirmed(
        CorrelationId.fromString("urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02").get,
        ComputedSegment(
          ProviderEndPoint("urn:ogf:network:nsa:internet2.edu",
            URI.create("http://oscars.internet2.edu/provider")),
          ServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE", new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSymmetricPath(true)
            .withSourceSTP("urn:ogf:network:internet2.edu:i2-edge?vlan=1780")
            .withDestSTP("urn:ogf:network:internet2.edu:to-esnet?vlan=1780")))
        :: ComputedSegment(
          ProviderEndPoint("urn:ogf:network:nsa:es.net",
            URI.create("http://oscars.es.net/nsi/ConnectionService")),
          ServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE", new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSymmetricPath(true)
            .withSourceSTP("urn:ogf:network:es.net:to-internet2?vlan=1780")
            .withDestSTP("urn:ogf:network:es.net:esnet-edge-one?vlan=1780")))
        :: Nil)

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize provider end point" in {
      Json.fromJson[ProviderEndPoint](Json.parse("""{"nsa":"urn:nsa:surfnet.nl", "csProviderURL":"http://localhost"}""")) must beEqualTo(
        JsSuccess(ProviderEndPoint("urn:nsa:surfnet.nl", URI.create("http://localhost"))))
    }

    "deserialize path computation confirmed with ero and parameters" in {
      val input = """
                    |{
                    |  "correlationId": "urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02",
                    |  "status": "SUCCESS",
                    |  "path": [
                    |        {
                    |            "nsa": "urn:ogf:network:kddilabs.jp:2013:nsa",
                    |            "csProviderURL": "http://210.196.65.114:9352/2013/07/connectionprovider",
                    |            "serviceType": "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
                    |            "p.p2ps": [
                    |              {
                    |                "capacity": 100,
                    |                "directionality": "Bidirectional",
                    |                "symmetricPath": true,
                    |                "sourceSTP": "urn:ogf:network:kddilabs.jp:2013:topology:bi-ps?vlan=1782",
                    |                "destSTP": "urn:ogf:network:kddilabs.jp:2013:topology:bi-kddilabs-jgn-x?vlan=1782",
                    |                "ero": {
                    |                   "orderedSTP": [
                    |                      {
                    |                        "@order": 0,
                    |                        "stp": "urn:ogf:network:kddilabs.jp:2013:topology:internalA"
                    |                      },
                    |                      {
                    |                        "@order": 1,
                    |                        "stp": "urn:ogf:network:kddilabs.jp:2013:topology:internalB"
                    |                      }
                    |                    ]
                    |                },
                    |                "parameter": [{"@type": "poopies", "value": "doodies"}]
                    |              }
                    |            ]
                    |        },
                    |        {
                    |            "nsa": "urn:ogf:network:netherlight.net:2013:nsa:safnari",
                    |            "csProviderURL": "https://agg.netherlight.net/nsi-v2/ConnectionServiceProvider",
                    |            "serviceType": "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
                    |            "p.p2ps": [
                    |              {
                    |                "capacity": 100,
                    |                "directionality": "Bidirectional",
                    |                "symmetricPath": true,
                    |                "sourceSTP": "urn:ogf:network:uvalight.net:2013:topology:netherlight?vlan=1782",
                    |                "destSTP": "urn:ogf:network:uvalight.net:2013:topology:ps?vlan=1782",
                    |                "ero": {
                    |                    "orderedSTP": [
                    |                      {
                    |                        "@order": 0,
                    |                        "stp": "urn:ogf:network:uvalight.net:2013:topology:internalA"
                    |                      },
                    |                      {
                    |                        "@order": 1,
                    |                        "stp": "urn:ogf:network:uvalight.net:2013:topology:internalB"
                    |                      }
                    |                    ]
                    |                },
                    |                "parameter": [{"@type": "poopies", "value": "doodies"}]
                    |              }
                    |            ]
                    |        }
                    |  ]
                    |}""".stripMargin

      val output = PathComputationConfirmed(
        CorrelationId.fromString("urn:uuid:f36d84dc-6713-4e27-a023-d753a80dcf02").get,
        ComputedSegment(
          ProviderEndPoint("urn:ogf:network:kddilabs.jp:2013:nsa",
            URI.create("http://210.196.65.114:9352/2013/07/connectionprovider")),
          ServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE", new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSymmetricPath(true)
            .withSourceSTP("urn:ogf:network:kddilabs.jp:2013:topology:bi-ps?vlan=1782")
            .withDestSTP("urn:ogf:network:kddilabs.jp:2013:topology:bi-kddilabs-jgn-x?vlan=1782")
            .withEro(new StpListType()
              .withOrderedSTP(new OrderedStpType().withOrder(0).withStp("urn:ogf:network:kddilabs.jp:2013:topology:internalA"))
              .withOrderedSTP(new OrderedStpType().withOrder(1).withStp("urn:ogf:network:kddilabs.jp:2013:topology:internalB")))
            .withParameter(new TypeValueType().withType("poopies").withValue("doodies"))
          ))
          :: ComputedSegment(
          ProviderEndPoint("urn:ogf:network:netherlight.net:2013:nsa:safnari",
            URI.create("https://agg.netherlight.net/nsi-v2/ConnectionServiceProvider")),
          ServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE", new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSymmetricPath(true)
            .withSourceSTP("urn:ogf:network:uvalight.net:2013:topology:netherlight?vlan=1782")
            .withDestSTP("urn:ogf:network:uvalight.net:2013:topology:ps?vlan=1782")
            .withEro(new StpListType()
              .withOrderedSTP(new OrderedStpType().withOrder(0).withStp("urn:ogf:network:uvalight.net:2013:topology:internalA"))
              .withOrderedSTP(new OrderedStpType().withOrder(1).withStp("urn:ogf:network:uvalight.net:2013:topology:internalB")))
            .withParameter(new TypeValueType().withType("poopies").withValue("doodies"))
          ))
          :: Nil)

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

  }
}
