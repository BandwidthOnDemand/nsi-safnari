package nl.surfnet.safnari

import java.net.URI
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.persistence.MessageStoreSpecification
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessagePersistenceSpec extends MessageStoreSpecification {
  import NsiMessageSpec._
  import Generators._

  private implicit val ArbitraryPathComputationAlgorithm: Arbitrary[PathComputationAlgorithm] = Arbitrary(Gen.oneOf(PathComputationAlgorithm.values))
  private implicit val ArbitraryServiceType: Arbitrary[ServiceType] = Arbitrary(Gen.resultOf(ServiceType.apply _))
  private implicit val ArbitraryPathComputationRequest: Arbitrary[PathComputationRequest] = Arbitrary(Gen.resultOf(PathComputationRequest.apply _))
  private implicit val ArbitraryPceRequest: Arbitrary[PceRequest] = Arbitrary(arbitrary[PathComputationRequest])

  override type Message = nl.surfnet.safnari.Message
  override implicit def MessageConversion = MessagePersistence.MessageToMessageData
  override implicit def MessageGenerator = Gen.oneOf(
    Gen.resultOf(ToPce.apply _),
    Gen.const(FromPce(PceMessageSpec.pathComputationResponse)),
    Gen.const(AckFromPce(PceMessageSpec.pathComputationFailedAck)),
    Gen.const(AckFromPce(PceMessageSpec.pathComputationAcceptedAck)),
    Gen.const(FromRequester(initialReserveMessage)),
    Gen.const(FromRequester(NsiProviderMessage(nsiProviderHeaders(InitialReserveCorrelationId, SessionSecurityAttr :: Nil), InitialReserve(InitialReserveType)))),
    Gen.const(FromProvider(NsiRequesterMessage(nsiRequesterHeaders(CorrelationId.random()), ReserveConfirmed(newConnectionId, ConfirmCriteria)))),
    Gen.const(ToProvider(initialReserveMessage, ProviderEndPoint("urn:nsa:surf", URI.create("http://localhost")))))
}
