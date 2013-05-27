package nl.surfnet.safnari

import play.api.test._
import play.api.db.DB
import anorm._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageStoreSpec extends helpers.Specification {
  def Application = FakeApplication(additionalConfiguration = testConfiguration)

  val messageStore = new MessageStore[Message]()

  "MessageStore" should {
    "store a PCE message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = PceMessageSpec.pathComputationRequest
      messageStore.store(aggregatedConnectionId, ToPce(message)) must not(beNull)

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case ToPce(request) => request must beEqualTo(message)
      }
    }

    "store a NSI message" in new WithApplication(Application) {
      val aggregatedConnectionId = newConnectionId
      val message = NsiMessageSpec.initialReserveMessage
      messageStore.store(aggregatedConnectionId, FromRequester(message)) must not(beNull)

      val loaded = messageStore.loadAll(aggregatedConnectionId)
      loaded must haveOneElementLike {
        case FromRequester(request) => request must beEqualTo(message)
      }
    }
  }
}
