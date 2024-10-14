package nl.surfnet.safnari

import java.net.URI
import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.persistence.MessageData
import nl.surfnet.nsiv2.persistence.MessageStoreSpecification
import nl.surfnet.nsiv2.soap.Conversion
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

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
    )
  )
end MessagePersistenceSpec
