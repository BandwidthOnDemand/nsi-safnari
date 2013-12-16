package nl.surfnet.safnari

import play.api.test._
import play.api.db.DB
import anorm._
import java.net.URI

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageStoreSpec extends helpers.Specification {
  sequential

  def Application = FakeApplication()

  val messageStore = new MessageStore[Message]()

  "MessageStore" should {

    "store a to PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationResponse
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(beEqualTo(FromPce(message))).exactly(1)
    }

    "store a from PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationResponse

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(beEqualTo(FromPce(message))).exactly(1)
    }

    "store a failed ack from PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationFailedAck

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, AckFromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(like[Message] {
        case ack @ AckFromPce(message) => ok
      }).exactly(1)
    }

    "store a accepted ack from PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationAcceptedAck

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, AckFromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(like[Message] {
        case ack @ AckFromPce(message) => ok
      }).exactly(1)
    }

    "store a fromRequester NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromRequester(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(beEqualTo(FromRequester(message))).exactly(1)
    }

    "store a fromProvider NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiRequesterMessage(NsiMessageSpec.headers(newCorrelationId, NsiHeaders.RequesterProtocolVersion), ReserveConfirmed(newConnectionId, NsiMessageSpec.ConfirmCriteria))

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromProvider(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(beEqualTo(FromProvider(message))).exactly(1)
    }

    "store a toProvider NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      val endPoint = ProviderEndPoint("urn:nsa:surf", URI.create("http://localhost"), NoAuthentication)

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, ToProvider(message, endPoint), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must contain(beEqualTo(ToProvider(message, endPoint))).exactly(1)
    }
  }

}
