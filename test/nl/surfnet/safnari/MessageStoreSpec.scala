package nl.surfnet.safnari

import play.api.test._
import play.api.db.DB
import anorm._

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageStoreSpec extends helpers.Specification {
  def Application = FakeApplication(additionalConfiguration = testConfiguration)

  "MessageStore" should {
    "store a message" in new WithApplication(Application) {
      val message = StoredMessage(newCorrelationId, "test-protocol", "test-message")
      new MessageStore[StoredMessage]().store(newConnectionId, message) must not(beNull)
    }

    "store an PCE message" in new WithApplication(Application) {
      val message = PceMessageSpec.pathComputationRequest
      new MessageStore[Either[NsiEnvelope[NsiMessage], PceMessage]]().store(newConnectionId, Right(message)) must not(beNull)

      DB.withConnection { implicit conn =>
        SQL("SELECT content FROM messages").apply().head[String]("content") must contain(message.correlationId.toString)
      }
    }
  }
}
