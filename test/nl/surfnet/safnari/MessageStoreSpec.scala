package nl.surfnet.safnari

import anorm._
import java.net.URI
import org.joda.time.Instant
import play.api.db.DB
import play.api.test._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageStoreSpec extends helpers.Specification {
  sequential

  class Fixture extends WithApplication() {
    lazy val timestamp = new Instant()
    lazy val aggregatedConnectionId = newConnectionId
    lazy val messageStore = {
      val s = new MessageStore()
      s.create(aggregatedConnectionId, timestamp, "requester_nsa")
      s
    }
  }

  "MessageStore" should {
    import NsiMessageSpec._

    "fail to store message for unknown connection" in new Fixture() {
      val unknownConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationResponse

      messageStore.storeInboundWithOutboundMessages(unknownConnectionId, timestamp, FromPce(message), Seq.empty) must throwA[IllegalArgumentException]
    }

    "fail to store a message for a deleted connection" in new Fixture() {
      messageStore.delete(aggregatedConnectionId, timestamp)

      val message = PceMessageSpec.pathComputationResponse
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromPce(message), Seq.empty) must throwA[IllegalArgumentException]
    }

    "store a to PCE message" in new Fixture() {
      val message = PceMessageSpec.pathComputationResponse
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded.map(_.createdAt) must contain(timestamp).exactly(1)
      loaded.map(_.message) must contain(beEqualTo(FromPce(message))).exactly(1)
    }

    "store a from PCE message" in new Fixture() {
      val message = PceMessageSpec.pathComputationResponse
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(beEqualTo(FromPce(message))).exactly(1)
    }

    "store a failed ack from PCE message" in new Fixture() {
      val message = PceMessageSpec.pathComputationFailedAck
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, AckFromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(beEqualTo(AckFromPce(message))).exactly(1)
    }

    "store a accepted ack from PCE message" in new Fixture() {
      val message = PceMessageSpec.pathComputationAcceptedAck
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, AckFromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(like[Message] {
        case ack @ AckFromPce(message) => ok
      }).exactly(1)
    }

    "store a fromRequester NSI message" in new Fixture() {
      val message = initialReserveMessage
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromRequester(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(beEqualTo(FromRequester(message))).exactly(1)
    }

    "store a fromRequester NSI message with session security attributes" in new Fixture() {
      val message = NsiProviderMessage(nsiProviderHeaders(InitialReserveCorrelationId, SessionSecurityAttr :: Nil), InitialReserve(InitialReserveType, ConfirmCriteria, Service))
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromRequester(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(beEqualTo(FromRequester(message))).exactly(1)
    }

    "store a fromProvider NSI message" in new Fixture() {
      val message = NsiRequesterMessage(nsiRequesterHeaders(newCorrelationId), ReserveConfirmed(newConnectionId, ConfirmCriteria))

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromProvider(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(beEqualTo(FromProvider(message))).exactly(1)
    }

    "store a toProvider NSI message" in new Fixture() {
      val message = initialReserveMessage
      val endPoint = ProviderEndPoint("urn:nsa:surf", URI.create("http://localhost"))
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, FromRequester(message), Seq(ToProvider(message, endPoint))) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId).map(_.message)
      loaded must contain(beEqualTo(ToProvider(message, endPoint))).exactly(1)
    }
  }
}
