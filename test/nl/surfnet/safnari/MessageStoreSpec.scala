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
      loaded must haveOneElementLike {
        case FromPce(request) => request must beEqualTo(message)
      }
    }

    "store a from PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationResponse

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromPce(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromPce(request) => request must beEqualTo(message)
      }
    }

    "store a fromRequester NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromRequester(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromRequester(request) => request must beEqualTo(message)
      }
    }

    "store a fromProvider NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiRequesterMessage(NsiMessageSpec.headers(newCorrelationId, NsiHeaders.RequesterProtocolVersion), ReserveConfirmed(newConnectionId, NsiMessageSpec.ConfirmCriteria))

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, FromProvider(message), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromProvider(request) => request must beEqualTo(message)
      }
    }

    "store a toProvider NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      val endPoint = ProviderEndPoint("urn:nsa:surf", URI.create("http://localhost"), NoAuthentication)

      messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, ToProvider(message, endPoint), Seq.empty) must not(throwA[Exception])

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case ToProvider(request, provider) =>
          request must beEqualTo(message)
          provider must beEqualTo(endPoint)
      }
    }
  }

}
