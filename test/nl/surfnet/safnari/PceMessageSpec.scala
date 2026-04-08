package nl.surfnet.safnari

import java.net.URI
import java.time.Instant

import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.*
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import play.api.libs.json.*

import nl.surfnet.nsiv2.messages.*
import javax.xml.datatype.DatatypeFactory
import javax.xml.namespace.QName
import javax.xml.XMLConstants

object PceMessageSpec:
  val sourceStp = "network-id:source"

  val destStp = "network-id:dest"
  val ServiceBaseType: P2PServiceBaseType = new P2PServiceBaseType()
    .withDirectionality(DirectionalityType.BIDIRECTIONAL)
    .withCapacity(100)
    .withSourceSTP(sourceStp)
    .withDestSTP(destStp)

  val ServiceTypeUrl = "http://services.ogf.org/nsi/2013/07/descriptions/EVTS.A-GOLE"

  val correlationId: CorrelationId = helpers.Specification.newCorrelationId()

  val pathComputationRequest: PathComputationRequest = PathComputationRequest(
    correlationId,
    Some("NSA-ID"),
    URI.create("http://localhost/pce/reply"),
    None,
    None,
    ServiceType(ServiceTypeUrl, ServiceBaseType),
    PathComputationAlgorithm.CHAIN,
    Nil
  )

  val providerEndPoint: ProviderEndPoint =
    ProviderEndPoint("provider-nsa", URI.create("http://localhost/pce/reply"))
  val computedSegment: ComputedSegment =
    ComputedSegment(providerEndPoint, ServiceType(ServiceTypeUrl, ServiceBaseType))
  val pathComputationResponse: PathComputationConfirmed =
    PathComputationConfirmed(correlationId, Seq(computedSegment))

  val pathComputationFailedAck: PceFailed = PceFailed(correlationId, 404, "Not Accepted", "")
  val pathComputationAcceptedAck: PceAccepted = PceAccepted(correlationId)
end PceMessageSpec

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PceMessageSpec extends helpers.Specification:
  import nl.surfnet.safnari.PceMessageSpec.*

  "PceMessages" should {

    import nl.surfnet.safnari.PceMessage.*

    "serialize request with p2pServiceBaseType to json" in {
      val request: PceRequest = PathComputationRequest(
        correlationId,
        Some("NSA-ID"),
        URI.create("http://localhost/pce/reply"),
        None,
        None,
        ServiceType(ServiceTypeUrl, ServiceBaseType),
        PathComputationAlgorithm.CHAIN,
        Nil
      )

      val json = Json.toJson(request)

      json \ "correlationId" must beEqualTo(JsDefined(JsString(correlationId.toString)))
      json \ "nsaId" must beEqualTo(JsDefined(JsString("NSA-ID")))
      json \ "replyTo" \ "url" must beEqualTo(JsDefined(JsString("http://localhost/pce/reply")))
      json \ "algorithm" must beEqualTo(JsDefined(JsString("CHAIN")))
      (json \ "p.p2ps" apply 0) \ "capacity" must beEqualTo(JsDefined(JsNumber(100)))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize request with connection trace to json" in {
      val first = new ConnectionType().withIndex(0).withValue("firstnsa")
      val second = new ConnectionType().withIndex(1).withValue("secondnsa")
      val connectionTrace = List(first, second)
      val request: PceRequest = PathComputationRequest(
        correlationId,
        Some("NSA-ID"),
        URI.create("http://localhost/pce/reply"),
        None,
        None,
        ServiceType(ServiceTypeUrl, ServiceBaseType),
        PathComputationAlgorithm.CHAIN,
        connectionTrace
      )

      val json = Json.toJson(request)
      (json \ "trace" apply 0) \ "index" must beEqualTo(JsDefined(JsNumber(first.getIndex)))
      (json \ "trace" apply 1) \ "index" must beEqualTo(JsDefined(JsNumber(second.getIndex)))
      (json \ "trace" apply 0) \ "value" must beEqualTo(JsDefined(JsString(first.getValue)))
      (json \ "trace" apply 1) \ "value" must beEqualTo(JsDefined(JsString(second.getValue)))

      Json.fromJson[PceRequest](json) must beEqualTo(JsSuccess(request))
    }

    "serialize computation failed response to json" in {
      val response = PathComputationFailed(correlationId, NsiError.NoServicePlanePathFound)

      val json = Json.toJson[PceResponse](response)

      json \ "correlationId" must beEqualTo(JsDefined(JsString(correlationId.toString)))

      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "serialize computation confirmed response to json" in {
      val response = PathComputationConfirmed(
        correlationId,
        List(ComputedSegment(providerEndPoint, ServiceType(ServiceTypeUrl, ServiceBaseType)))
      )

      val json = Json.toJson[PceResponse](response)

      json \ "correlationId" must beEqualTo(JsDefined(JsString(correlationId.toString)))
      Json.fromJson[PceResponse](json) must beEqualTo(JsSuccess(response))
    }

    "deserialize path computation failed" in {
      val input =
        """{"correlationId":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED",
            "m.findPathError":{"code":"00702","label":"LABEL","description":"oops!","variable":{"@type":"type", "value":"value"}}}""".stripMargin

      val output = PathComputationFailed(
        CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get,
        NsiError("00702", "LABEL", "oops!", Seq(new QName("type") -> "value"))
      )

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize path computation failed from version 1 message" in {
      val input =
        """{"correlationId":"urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640","status":"FAILED", "message": "oops!"}""".stripMargin

      val output = PathComputationFailed(
        CorrelationId.fromString("urn:uuid:e679ca48-ec51-4d7d-a24f-e23eca170640").get,
        NsiError.TopologyError.copy(text = "oops!")
      )

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
          ProviderEndPoint(
            "urn:ogf:network:nsa:internet2.edu",
            URI.create("http://oscars.internet2.edu/provider")
          ),
          ServiceType(
            "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
            new P2PServiceBaseType()
              .withCapacity(100)
              .withDirectionality(DirectionalityType.BIDIRECTIONAL)
              .withSymmetricPath(true)
              .withSourceSTP("urn:ogf:network:internet2.edu:i2-edge?vlan=1780")
              .withDestSTP("urn:ogf:network:internet2.edu:to-esnet?vlan=1780")
          )
        )
          :: ComputedSegment(
            ProviderEndPoint(
              "urn:ogf:network:nsa:es.net",
              URI.create("http://oscars.es.net/nsi/ConnectionService")
            ),
            ServiceType(
              "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
              new P2PServiceBaseType()
                .withCapacity(100)
                .withDirectionality(DirectionalityType.BIDIRECTIONAL)
                .withSymmetricPath(true)
                .withSourceSTP("urn:ogf:network:es.net:to-internet2?vlan=1780")
                .withDestSTP("urn:ogf:network:es.net:esnet-edge-one?vlan=1780")
            )
          )
          :: Nil
      )

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

    "deserialize provider end point" in {
      Json.fromJson[ProviderEndPoint](
        Json.parse("""{"nsa":"urn:nsa:surfnet.nl", "csProviderURL":"http://localhost"}""")
      ) must beEqualTo(
        JsSuccess(ProviderEndPoint("urn:nsa:surfnet.nl", URI.create("http://localhost")))
      )
    }

    "round-trip request with all optional fields absent" in {
      val request: PceRequest = PathComputationRequest(
        correlationId,
        None,
        URI.create("http://localhost/pce/reply"),
        None,
        None,
        ServiceType(ServiceTypeUrl, ServiceBaseType),
        PathComputationAlgorithm.CHAIN,
        Nil
      )

      val json = Json.toJson(request)
      val parsed = Json.fromJson[PceRequest](json)

      parsed must beLike { case JsSuccess(r: PathComputationRequest, _) =>
        r.nsaId must beNone
        r.startTime must beNone
        r.endTime must beNone
      }
    }

    "round-trip request with all optional fields present" in {
      val now = Instant.now()
      val later = now.plusSeconds(3600)
      val request: PceRequest = PathComputationRequest(
        correlationId,
        Some("urn:ogf:network:example.net:2025:nsa"),
        URI.create("http://localhost/pce/reply"),
        Some(now),
        Some(later),
        ServiceType(ServiceTypeUrl, ServiceBaseType),
        PathComputationAlgorithm.CHAIN,
        Nil
      )

      val json = Json.toJson(request)
      val parsed = Json.fromJson[PceRequest](json)

      parsed must beLike { case JsSuccess(r: PathComputationRequest, _) =>
        r.nsaId must beSome("urn:ogf:network:example.net:2025:nsa")
        r.startTime must beSome
        r.endTime must beSome
        r.startTime.get.toEpochMilli must beEqualTo(now.toEpochMilli)
        r.endTime.get.toEpochMilli must beEqualTo(later.toEpochMilli)
      }
    }

    "round-trip request with large capacity value" in {
      val largeCapacityService = new P2PServiceBaseType()
        .withDirectionality(DirectionalityType.BIDIRECTIONAL)
        .withCapacity(Long.MaxValue)
        .withSourceSTP(PceMessageSpec.sourceStp)
        .withDestSTP(PceMessageSpec.destStp)
      val request: PceRequest = PathComputationRequest(
        correlationId,
        Some("NSA-ID"),
        URI.create("http://localhost/pce/reply"),
        None,
        None,
        ServiceType(ServiceTypeUrl, largeCapacityService),
        PathComputationAlgorithm.CHAIN,
        Nil
      )

      val json = Json.toJson(request)
      val parsed = Json.fromJson[PceRequest](json)

      parsed must beLike { case JsSuccess(r: PathComputationRequest, _) =>
        r.serviceType.service.getCapacity must beEqualTo(Long.MaxValue)
      }
    }

    "round-trip P2PServiceBaseType with null directionality defaults to Bidirectional" in {
      val json = Json.parse("""{
        "capacity": 100,
        "sourceSTP": "urn:stp:a",
        "destSTP": "urn:stp:b",
        "parameter": []
      }""")

      val parsed = Json.fromJson[P2PServiceBaseType](json)
      parsed must beLike { case JsSuccess(svc, _) =>
        svc.getDirectionality must beEqualTo(DirectionalityType.BIDIRECTIONAL)
        svc.isSymmetricPath must beNull
        svc.getEro must beNull
      }
    }

    "round-trip P2PServiceBaseType with symmetricPath boolean" in {
      val json = Json.parse("""{
        "capacity": 100,
        "directionality": "Bidirectional",
        "symmetricPath": true,
        "sourceSTP": "urn:stp:a",
        "destSTP": "urn:stp:b",
        "parameter": []
      }""")

      val parsed = Json.fromJson[P2PServiceBaseType](json)
      parsed must beLike { case JsSuccess(svc, _) =>
        svc.isSymmetricPath must beEqualTo(java.lang.Boolean.TRUE)

        val roundTripped =
          Json.fromJson[P2PServiceBaseType](Json.toJson(svc)(pointToPointServiceFormat))
        roundTripped must beLike { case JsSuccess(svc2, _) =>
          svc2.isSymmetricPath must beEqualTo(java.lang.Boolean.TRUE)
        }
      }
    }

    "round-trip P2PServiceBaseType with symmetricPath false" in {
      val json = Json.parse("""{
        "capacity": 100,
        "directionality": "Bidirectional",
        "symmetricPath": false,
        "sourceSTP": "urn:stp:a",
        "destSTP": "urn:stp:b",
        "parameter": []
      }""")

      val parsed = Json.fromJson[P2PServiceBaseType](json)
      parsed must beLike { case JsSuccess(svc, _) =>
        svc.isSymmetricPath must beEqualTo(java.lang.Boolean.FALSE)
      }
    }

    "round-trip NsiError with empty variables" in {
      val error = NsiError.NoServicePlanePathFound

      val json = Json.toJson[PceResponse](PathComputationFailed(correlationId, error))
      val parsed = Json.fromJson[PceResponse](json)
      parsed must beLike { case JsSuccess(PathComputationFailed(_, e), _) =>
        e.id must beEqualTo(error.id)
        e.description must beEqualTo(error.description)
        e.text must beEqualTo(error.text)
      }
    }

    "round-trip NsiError with namespace and variable" in {
      val error =
        NsiError(
          "00702",
          "LABEL",
          "oops!",
          Seq(new QName("http://example.com/ns", "localName") -> "value")
        )

      val json = Json.toJson[PceResponse](PathComputationFailed(correlationId, error))
      val parsed = Json.fromJson[PceResponse](json)
      parsed must beLike { case JsSuccess(PathComputationFailed(_, e), _) =>
        e.variables must have size 1
        e.variables.head._1.getLocalPart must beEqualTo("localName")
        e.variables.head._1.getNamespaceURI must beEqualTo("http://example.com/ns")
        e.variables.head._2 must beEqualTo("value")
      }
    }

    "round-trip NsiError with null namespace variable" in {
      val error =
        NsiError(
          "00702",
          "LABEL",
          "oops!",
          Seq(new QName(XMLConstants.NULL_NS_URI, "localPart") -> "value")
        )

      val json = Json.toJson[PceResponse](PathComputationFailed(correlationId, error))
      val parsed = Json.fromJson[PceResponse](json)
      parsed must beLike { case JsSuccess(PathComputationFailed(_, e), _) =>
        e.variables must have size 1
        e.variables.head._1.getLocalPart must beEqualTo("localPart")
      }
    }

    "round-trip XMLGregorianCalendar through JSON" in {
      val cal = DatatypeFactory.newInstance().newXMLGregorianCalendar("2025-06-15T10:30:00.000Z")
      val json = Json.toJson(cal)
      json must beEqualTo(JsString("2025-06-15T10:30:00.000Z"))

      val parsed = Json.fromJson[javax.xml.datatype.XMLGregorianCalendar](json)
      parsed must beLike { case JsSuccess(c, _) =>
        c.toXMLFormat must beEqualTo("2025-06-15T10:30:00.000Z")
      }
    }

    "deserialize PceAccepted from status 202" in {
      val json = Json.parse(s"""{"correlationId":"${correlationId}","status":202}""")
      val parsed = Json.fromJson[PceAcknowledgement](json)
      parsed must beLike { case JsSuccess(PceAccepted(id), _) =>
        id must beEqualTo(correlationId)
      }
    }

    "deserialize PceFailed from non-202 status" in {
      val json = Json.parse(
        s"""{"correlationId":"${correlationId}","status":500,"statusText":"Internal Server Error","message":"Something went wrong"}"""
      )
      val parsed = Json.fromJson[PceAcknowledgement](json)
      parsed must beLike { case JsSuccess(PceFailed(id, status, statusText, message), _) =>
        id must beEqualTo(correlationId)
        status must beEqualTo(500)
        statusText must beEqualTo("Internal Server Error")
        message must beEqualTo("Something went wrong")
      }
    }

    "round-trip PceAcknowledgement for accepted" in {
      val ack: PceAcknowledgement = PceAccepted(correlationId)
      val json = Json.toJson(ack)
      val parsed = Json.fromJson[PceAcknowledgement](json)
      parsed must beLike { case JsSuccess(PceAccepted(id), _) =>
        id must beEqualTo(correlationId)
      }
    }

    "round-trip PceAcknowledgement for failed" in {
      val ack: PceAcknowledgement = PceFailed(correlationId, 404, "Not Found", "Path not found")
      val json = Json.toJson(ack)
      val parsed = Json.fromJson[PceAcknowledgement](json)
      parsed must beLike { case JsSuccess(PceFailed(id, status, statusText, message), _) =>
        id must beEqualTo(correlationId)
        status must beEqualTo(404)
        statusText must beEqualTo("Not Found")
        message must beEqualTo("Path not found")
      }
    }

    "round-trip all PathComputationAlgorithm values" in {
      org.specs2.execute.Result.foreach(PathComputationAlgorithm.values.toSeq) { algo =>
        val request: PceRequest = PathComputationRequest(
          correlationId,
          None,
          URI.create("http://localhost/pce/reply"),
          None,
          None,
          ServiceType(ServiceTypeUrl, ServiceBaseType),
          algo,
          Nil
        )
        val json = Json.toJson(request)
        val parsed = Json.fromJson[PceRequest](json)
        parsed must beLike { case JsSuccess(r: PathComputationRequest, _) =>
          r.algorithm must beEqualTo(algo)
        }
      }
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
          ProviderEndPoint(
            "urn:ogf:network:kddilabs.jp:2013:nsa",
            URI.create("http://210.196.65.114:9352/2013/07/connectionprovider")
          ),
          ServiceType(
            "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
            new P2PServiceBaseType()
              .withCapacity(100)
              .withDirectionality(DirectionalityType.BIDIRECTIONAL)
              .withSymmetricPath(true)
              .withSourceSTP("urn:ogf:network:kddilabs.jp:2013:topology:bi-ps?vlan=1782")
              .withDestSTP("urn:ogf:network:kddilabs.jp:2013:topology:bi-kddilabs-jgn-x?vlan=1782")
              .withEro(
                new StpListType()
                  .withOrderedSTP(
                    new OrderedStpType()
                      .withOrder(0)
                      .withStp("urn:ogf:network:kddilabs.jp:2013:topology:internalA")
                  )
                  .withOrderedSTP(
                    new OrderedStpType()
                      .withOrder(1)
                      .withStp("urn:ogf:network:kddilabs.jp:2013:topology:internalB")
                  )
              )
              .withParameter(new TypeValueType().withType("poopies").withValue("doodies"))
          )
        )
          :: ComputedSegment(
            ProviderEndPoint(
              "urn:ogf:network:netherlight.net:2013:nsa:safnari",
              URI.create("https://agg.netherlight.net/nsi-v2/ConnectionServiceProvider")
            ),
            ServiceType(
              "http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE",
              new P2PServiceBaseType()
                .withCapacity(100)
                .withDirectionality(DirectionalityType.BIDIRECTIONAL)
                .withSymmetricPath(true)
                .withSourceSTP("urn:ogf:network:uvalight.net:2013:topology:netherlight?vlan=1782")
                .withDestSTP("urn:ogf:network:uvalight.net:2013:topology:ps?vlan=1782")
                .withEro(
                  new StpListType()
                    .withOrderedSTP(
                      new OrderedStpType()
                        .withOrder(0)
                        .withStp("urn:ogf:network:uvalight.net:2013:topology:internalA")
                    )
                    .withOrderedSTP(
                      new OrderedStpType()
                        .withOrder(1)
                        .withStp("urn:ogf:network:uvalight.net:2013:topology:internalB")
                    )
                )
                .withParameter(new TypeValueType().withType("poopies").withValue("doodies"))
            )
          )
          :: Nil
      )

      Json.fromJson[PceResponse](Json.parse(input)) must beEqualTo(JsSuccess(output))
    }

  }
end PceMessageSpec
