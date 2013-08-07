package nl.surfnet.safnari

import play.api.test._
import play.api.db.DB
import anorm._
import java.net.URI

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageStoreSpec extends helpers.Specification {
  def Application = FakeApplication(additionalConfiguration = testConfiguration)

  val messageStore = new MessageStore[Message]()

  "MessageStore" should {

    "store a to PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationRequest
      messageStore.store(aggregatedConnectionId, ToPce(message)) must not(beNull)

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case ToPce(request) => request must beEqualTo(message)
      }
    }

    "store a from PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationResponse

      messageStore.store(aggregatedConnectionId, FromPce(message)) must not(beNull)

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromPce(request) => request must beEqualTo(message)
      }
    }

    "store a fromRequester NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      messageStore.store(aggregatedConnectionId, FromRequester(message)) must beSome

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromRequester(request) => request must beEqualTo(message)
      }
    }

    "store a fromProvider NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = ReserveConfirmed(NsiMessageSpec.headers(newCorrelationId), newConnectionId, NsiMessageSpec.ConfirmCriteria)

      messageStore.store(aggregatedConnectionId, FromProvider(message)) must not(beNull)

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromProvider(request) => request must beEqualTo(message)
      }
    }

    "store a toProvider NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      val endPoint = ProviderEndPoint("urn:nsa:surf", URI.create("http://localhost"), NoAuthentication)

      messageStore.store(aggregatedConnectionId, ToProvider(message, endPoint)) must not(beNull)

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case ToProvider(request, provider) =>
          request must beEqualTo(message)
          provider must beEqualTo(endPoint)
      }
    }
  }

}
