package nl.surfnet.safnari

import java.net.URI
import java.time.Instant
import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.persistence.MessageData
import nl.surfnet.nsiv2.persistence.MessageStoreSpecification
import nl.surfnet.nsiv2.soap.Conversion
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import play.api.libs.json.*
import scala.util.Success

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessagePersistenceSpec extends MessageStoreSpecification:
  import helpers.NsiMessages.*
  import Generators.given

  given Arbitrary[PathComputationAlgorithm] = Arbitrary(
    Gen.oneOf(PathComputationAlgorithm.values.toIndexedSeq)
  )
  given Arbitrary[ServiceType] = Arbitrary(Gen.resultOf(ServiceType.apply _))
  given Arbitrary[PathComputationRequest] = Arbitrary(Gen.resultOf(PathComputationRequest.apply _))
  given Arbitrary[PceRequest] = Arbitrary(arbitrary[PathComputationRequest])

  override type Message = nl.surfnet.safnari.Message
  override given MessageConversion: Conversion[Message, MessageData] =
    MessagePersistence.MessageToMessageData
  override given MessageGenerator: Gen[Message] = Gen.oneOf(
    Gen.resultOf(ToPce.apply _),
    Gen.const(FromPce(PceMessageSpec.pathComputationResponse)),
    Gen.const(AckFromPce(PceMessageSpec.pathComputationFailedAck)),
    Gen.const(AckFromPce(PceMessageSpec.pathComputationAcceptedAck)),
    Gen.const(FromRequester(initialReserveMessage)),
    Gen.const(
      FromRequester(
        NsiProviderMessage(
          nsiProviderHeaders(A.provider, InitialReserveCorrelationId, SessionSecurityAttr :: Nil),
          InitialReserve(InitialReserveType)
        )
      )
    ),
    Gen.const(
      FromProvider(
        NsiRequesterMessage(
          nsiRequesterHeaders(CorrelationId.random()),
          ReserveConfirmed(newConnectionId(), ConfirmCriteria)
        )
      )
    ),
    Gen.const(
      ToProvider(
        initialReserveMessage,
        ProviderEndPoint("urn:nsa:surf", URI.create("http://localhost"))
      )
    ),
    Gen.const(
      MessageDeliveryFailure(
        CorrelationId.random(),
        Some(newConnectionId()),
        CorrelationId.random(),
        URI.create("https://example.com/endpoint"),
        Instant.now(),
        "message-delivery-timeout"
      )
    ),
    Gen.const(
      MessageDeliveryFailure(
        CorrelationId.random(),
        None,
        CorrelationId.random(),
        URI.create("https://example.com/endpoint"),
        Instant.now(),
        "message-delivery-timeout"
      )
    ),
    Gen.const(
      PassedEndTime(
        CorrelationId.random(),
        newConnectionId(),
        Instant.now()
      )
    )
  )

  "MessageDeliveryFailure" should {
    "round-trip through JSON with connectionId present" in {
      import MessagePersistence.MessageDeliveryFailureFormat
      val msg = MessageDeliveryFailure(
        CorrelationId.random(),
        Some("conn-1"),
        CorrelationId.random(),
        URI.create("https://example.com/endpoint"),
        Instant.now(),
        "message-delivery-timeout"
      )
      val json = Json.toJson(msg)
      val parsed = Json.fromJson[MessageDeliveryFailure](json)
      parsed must beLike { case JsSuccess(m, _) =>
        m.connectionId must beEqualTo(msg.connectionId)
        m.uri must beEqualTo(msg.uri)
        m.timestamp.toEpochMilli must beEqualTo(msg.timestamp.toEpochMilli)
        m.message must beEqualTo(msg.message)
      }
    }

    "round-trip through JSON with connectionId absent" in {
      import MessagePersistence.MessageDeliveryFailureFormat
      val msg = MessageDeliveryFailure(
        CorrelationId.random(),
        None,
        CorrelationId.random(),
        URI.create("https://example.com/endpoint"),
        Instant.now(),
        "message-delivery-timeout"
      )
      val json = Json.toJson(msg)
      val parsed = Json.fromJson[MessageDeliveryFailure](json)
      parsed must beLike { case JsSuccess(m, _) =>
        m.connectionId must beNone
      }
    }

    "round-trip through MessageData conversion" in {
      val msg: Message = MessageDeliveryFailure(
        CorrelationId.random(),
        Some("conn-2"),
        CorrelationId.random(),
        URI.create("https://example.com/endpoint"),
        Instant.now(),
        "message-delivery-timeout"
      )
      val converted = MessagePersistence.MessageToMessageData(msg)
      converted must beLike { case Success(data) =>
        data.tpe must beEqualTo("MessageDeliveryFailure")
        val restored = MessagePersistence.MessageToMessageData.invert(data)
        restored must beLike { case Success(m: MessageDeliveryFailure) =>
          m.connectionId must beEqualTo(Some("conn-2"))
          m.timestamp.toEpochMilli must beEqualTo(
            msg.asInstanceOf[MessageDeliveryFailure].timestamp.toEpochMilli
          )
        }
      }
    }
  }

  "PassedEndTime" should {
    "round-trip through JSON" in {
      import MessagePersistence.PassedEndTimeFormat
      val msg = PassedEndTime(
        CorrelationId.random(),
        "conn-3",
        Instant.now()
      )
      val json = Json.toJson(msg)
      val parsed = Json.fromJson[PassedEndTime](json)
      parsed must beLike { case JsSuccess(m, _) =>
        m.connectionId must beEqualTo(msg.connectionId)
        m.timestamp.toEpochMilli must beEqualTo(msg.timestamp.toEpochMilli)
      }
    }

    "round-trip through MessageData conversion" in {
      val msg: Message = PassedEndTime(
        CorrelationId.random(),
        "conn-4",
        Instant.now()
      )
      val converted = MessagePersistence.MessageToMessageData(msg)
      converted must beLike { case Success(data) =>
        data.tpe must beEqualTo("PassedEndTime")
        val restored = MessagePersistence.MessageToMessageData.invert(data)
        restored must beLike { case Success(m: PassedEndTime) =>
          m.connectionId must beEqualTo("conn-4")
        }
      }
    }
  }

  "Instant serialization" should {
    "round-trip at epoch boundaries" in {
      import MessagePersistence.PassedEndTimeFormat
      val instants = Seq(
        Instant.EPOCH,
        Instant.now(),
        Instant.parse("2099-12-31T23:59:59Z")
      )
      org.specs2.execute.Result.foreach(instants) { instant =>
        val msg = PassedEndTime(CorrelationId.random(), "conn-test", instant)
        val json = Json.toJson(msg)
        val parsed = Json.fromJson[PassedEndTime](json)
        parsed must beLike { case JsSuccess(m, _) =>
          m.timestamp.toEpochMilli must beEqualTo(instant.toEpochMilli)
        }
      }
    }
  }
end MessagePersistenceSpec
