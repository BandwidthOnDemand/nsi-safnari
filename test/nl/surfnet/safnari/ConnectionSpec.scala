package nl.surfnet.safnari

import java.util.UUID
import java.net.URI
import javax.xml.datatype.DatatypeFactory
import scala.collection.JavaConverters._
import org.specs2.specification.Scope
import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._07.framework.types.TypeValuePairListType
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends helpers.Specification {
  trait fixture extends Scope {

    val Service = new P2PServiceBaseType().withCapacity(100).withSourceSTP(new StpType().withLocalId("A")).withDestSTP(new StpType().withLocalId("B"))
    val Schedule = new ScheduleType()
    val Criteria = new ReservationConfirmCriteriaType().withSchedule(Schedule).withServiceType("ServiceType").withPointToPointService(Service)
    val InitialReserveType = new ReserveType().withCriteria(Conversion.convert(Criteria).right.get)
    val A = ComputedSegment(
        new P2PServiceBaseType().withCapacity(100).withSourceSTP(new StpType().withLocalId("A")).withDestSTP(new StpType().withLocalId("X")),
        ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"), NoAuthentication))
    val B = ComputedSegment(
        new P2PServiceBaseType().withCapacity(100).withSourceSTP(new StpType().withLocalId("X")).withDestSTP(new StpType().withLocalId("B")),
        ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider"), NoAuthentication))

    val AggregatorNsa = "urn:ogf:network:nsa:surfnet-nsi-safnari"
    val Headers = NsiHeaders(newCorrelationId, "RequesterNSA", AggregatorNsa, Some(URI.create("http://example.com/")), NsiHeaders.ProviderProtocolVersion)
    val ConnectionId = "ConnectionId"
    val ReserveCorrelationId = newCorrelationId
    val CommitCorrelationId = newCorrelationId

    val InitialReserveMessage = ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, Criteria, Service)).message.asInstanceOf[NsiProviderMessage[InitialReserve]]
    val InitialReserveHeaders = InitialReserveMessage.headers
    val InitialMessages = Seq(FromRequester(InitialReserveMessage))

    val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    val NsiReplyToUri = URI.create("http://example.com/nsi/requester")
    val PceReplyToUri = URI.create("http://example.com/pce/reply")

    def toProviderHeaders(provider: ProviderEndPoint, correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, provider.nsa, Some(NsiReplyToUri), NsiHeaders.ProviderProtocolVersion)
    def toRequesterHeaders(correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, "RequesterNSA", None, NsiHeaders.RequesterProtocolVersion)

    val connection = new ConnectionEntity(ConnectionId, InitialReserveMessage, () => CorrelationId.fromUuid(mockUuidGenerator()), AggregatorNsa, NsiReplyToUri, PceReplyToUri)

    def given(messages: InboundMessage*): Unit = messages.foreach(connection.process)

    var messages: Seq[Message] = Nil
    def when(message: InboundMessage): Option[Seq[Message]] = {
      messages = Nil
      connection.process(message).tap(_.foreach(messages = _))
    }

    def connectionData = connection.query

    def reservationState = connectionData.getConnectionStates().getReservationState()
    def provisionState = connectionData.getConnectionStates().getProvisionState()
    def lifecycleState = connectionData.getConnectionStates().getLifecycleState()
    def dataPlaneStatus = connectionData.getConnectionStates().getDataPlaneStatus()

    object ura {
      def request(correlationId: CorrelationId, operation: NsiProviderOperation) = {
        val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, Some(URI.create("http://ultimate.requester.example.com/")), NsiHeaders.ProviderProtocolVersion)
        FromRequester(NsiProviderMessage(headers, operation))
      }
    }
    object upa {
      def response(correlationId: CorrelationId, operation: NsiRequesterOperation) = {
        val headers = NsiHeaders(correlationId, AggregatorNsa, "ProviderNSA", None, NsiHeaders.RequesterProtocolVersion)
        FromProvider(NsiRequesterMessage(headers, operation))
      }
      def notification(correlationId: CorrelationId, operation: NsiRequesterOperation) = {
        val headers = NsiHeaders(correlationId, AggregatorNsa, "ProviderNSA", None, NsiHeaders.RequesterProtocolVersion)
        FromProvider(NsiRequesterMessage(headers, operation))
      }
    }
    object agg {
      def response(correlationId: CorrelationId, operation: NsiRequesterOperation) = {
        val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
        ToRequester(NsiRequesterMessage(headers, operation))
      }
      def notification(correlationId: CorrelationId, operation: NsiNotification) = {
        val headers = NsiHeaders(correlationId, "RequesterNSA", AggregatorNsa, None, NsiHeaders.RequesterProtocolVersion)
        ToRequester(NsiRequesterMessage(headers, operation))
      }
    }
  }

  trait ReservedConnection extends fixture {
    given(
      ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, Criteria, Service)),
      FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
      upa.response(CorrelationId(0, 2), ReserveConfirmed("ConnectionIdA", Criteria)),
      ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)),
      upa.response(CorrelationId(0, 3), ReserveCommitConfirmed("ConnectionIdA")))
  }

  trait Released { this: ReservedConnection =>
  }

  trait Provisioned { this: ReservedConnection =>
    val ProvisionCorrelationId = newCorrelationId

    given(
      ura.request(ProvisionCorrelationId, Provision(ConnectionId)),
      upa.response(newCorrelationId, ProvisionConfirmed("ConnectionIdA")))
  }

  "A connection" should {
    "send a path computation request when reserve is received" in new fixture {
      when(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, Criteria, Service)))

      messages must contain(ToPce(PathComputationRequest(
        correlationId = CorrelationId(0, 1),
        replyTo = PceReplyToUri,
        schedule = Schedule,
        service = Service)))
    }

    "send reserve request for each segment when path computation confirmed is received" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, Criteria, Service)))

      when(FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))))

      messages must haveSize(2)
      messages must haveAllElementsLike {
        case ToProvider(NsiProviderMessage(_, reserve: InitialReserve), A.provider) => ok
        case ToProvider(NsiProviderMessage(_, reserve: InitialReserve), B.provider) => ok
      }
    }

    "fail the connection when path computation fails" in new fixture {
      given(ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, Criteria, Service)))

      when(FromPce(PathComputationFailed(CorrelationId(0, 1), "failed")))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(Headers.copy(correlationId = ReserveCorrelationId).forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.PathComputationNoPath.id)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "confirm the reservation with a single path segment" in new fixture {
      given(
        ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType, Criteria, Service)),
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))))

      when(upa.response(CorrelationId(0, 2), ReserveConfirmed("connectionId", Criteria)))

      messages must contain(ToRequester(NsiRequesterMessage(InitialReserveHeaders.forAsyncReply, ReserveConfirmed(ConnectionId, Criteria))))
      messages must contain(agg.response(ReserveCorrelationId, ReserveConfirmed(ConnectionId, Criteria)))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
    }

    "be in reservation held state when both segments are confirmed" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B)))): _*)

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveConfirmed("ConnectionIdA", Criteria))))
      messages must beEmpty

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 3)), ReserveConfirmed("ConnectionIdB", Criteria))))
      messages must contain(ToRequester(NsiRequesterMessage(InitialReserveHeaders.forAsyncReply, ReserveConfirmed(ConnectionId, Criteria))))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
    }

    "fail the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A)))): _*)

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(A.provider.nsa))))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(1)
          failed.getServiceException().getChildException().get(0).getErrorId() must beEqualTo(NsiError.BandwidthNotAvailable.id)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "fail the reservation with two segments and at least one fails" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))),
        FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(A.provider.nsa)))))): _*)

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 3)), ReserveConfirmed("connectionIdB", Criteria))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(1)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "fail the reservation with two segments when both fail" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))),
        FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(A.provider.nsa)))))): _*)

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 3)), ReserveFailed(new GenericFailedType().withConnectionId("ConnectionIdB").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(B.provider.nsa))))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(NsiRequesterMessage(headers, ReserveFailed(failed))) =>
          headers must beEqualTo(InitialReserveHeaders.forAsyncReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(2)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "be in committing state when reserve commit is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveConfirmed(ConnectionId, Criteria)))): _*)

      when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = newCorrelationId), ReserveCommit(ConnectionId))))

      messages must haveOneElementLike { case ToProvider(NsiProviderMessage(_, _: ReserveCommit), _) => ok }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_COMMITTING)
    }

    "be in reserved state when reserve commit confirmed is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveConfirmed("ConnectionIdA", Criteria))),
        FromRequester(NsiProviderMessage(Headers.copy(correlationId = CommitCorrelationId), ReserveCommit(ConnectionId)))): _*)

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 3)), ReserveCommitConfirmed("ConnectionIdA"))))

      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = CommitCorrelationId).forAsyncReply, ReserveCommitConfirmed(ConnectionId))))
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
    }

    "reject commit when in initial state" in new fixture {
      val ack = when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = CommitCorrelationId), ReserveCommit(ConnectionId))))

      ack must beNone
    }

    "be in aborting state when reserve abort is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveConfirmed("ConnectionIdA", Criteria)))): _*)

      when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = ReserveCorrelationId), ReserveAbort(ConnectionId))))

      messages must haveOneElementLike { case ToProvider(NsiProviderMessage(_, _: ReserveAbort), _) => ok }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_ABORTING)
    }

    "provide information about connections" in new fixture {
      given(InitialMessages: _*)

      val result = connection.query

      result.getConnectionId() must beEqualTo(ConnectionId)
      result.getCriteria().get(0).getChildren().getChild() must haveSize(0)
    }

    "provide information about connections with children" in new ReservedConnection {
      given(InitialMessages: _*)

      val result = connection.query

      result.getCriteria().get(0).getChildren().getChild() must haveSize(1)
    }

    "be in released state when initial reserve" in new fixture {
      given(InitialMessages: _*)

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
    }

    "initialize the provisioning state machine when reservation is confirmed" in new fixture {
      given(InitialMessages :+ FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))): _*)

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 2)), ReserveConfirmed("ConnectionIdA", Criteria))))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
    }

    "become provisioning on provision request" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = ProvisionCorrelationId), Provision(ConnectionId))))

      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 5)), Provision("ConnectionIdA")), A.provider))
    }

    "send a provision confirmed to requester" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      given(FromRequester(NsiProviderMessage(Headers.copy(correlationId = ProvisionCorrelationId), Provision(ConnectionId))))

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 4)), ProvisionConfirmed("ConnectionIdA"))))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONED)
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = ProvisionCorrelationId).forAsyncReply, ProvisionConfirmed(ConnectionId))))
    }

    "become releasing on release request" in new ReservedConnection with Provisioned {
      val ReleaseCorrelationId = newCorrelationId

      when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = ReleaseCorrelationId), Release(ConnectionId))))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASING)
      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 6)), Release("ConnectionIdA")), A.provider))
    }

    "send release confirmed to requester" in new ReservedConnection with Provisioned {
      val ReleaseCorrelationId = newCorrelationId

      given(FromRequester(NsiProviderMessage(Headers.copy(correlationId = ReleaseCorrelationId), Release(ConnectionId))))

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 5)), ReleaseConfirmed("ConnectionIdA"))))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = ReleaseCorrelationId).forAsyncReply, ReleaseConfirmed(ConnectionId))))
    }

    "reject release request when released" in new ReservedConnection with Released {
      val ack = when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = newCorrelationId), Release(ConnectionId))))

      ack must beNone
    }

    "reject provision request when provisioned" in new ReservedConnection with Provisioned {
      val ack = when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = newCorrelationId), Provision(ConnectionId))))

      ack must beNone
    }

    "become terminating on terminate request" in new ReservedConnection {
      val TerminateCorrelationId = newCorrelationId

      when(FromRequester(NsiProviderMessage(Headers.copy(correlationId = TerminateCorrelationId), Terminate(ConnectionId))))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      messages must contain(ToProvider(NsiProviderMessage(toProviderHeaders(A.provider, CorrelationId(0, 5)), Terminate("ConnectionIdA")), A.provider))
    }

    "send a terminate confirmed to requester" in new ReservedConnection {
      val TerminateCorrelationId = newCorrelationId

      given(FromRequester(NsiProviderMessage(Headers.copy(correlationId = TerminateCorrelationId), Terminate(ConnectionId))))

      when(FromProvider(NsiRequesterMessage(Headers.copy(correlationId = CorrelationId(0, 4)), TerminateConfirmed("ConnectionIdA"))))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
      messages must contain(ToRequester(NsiRequesterMessage(Headers.copy(correlationId = TerminateCorrelationId).forAsyncReply, TerminateConfirmed(ConnectionId))))
    }

    "have a data plane inactive" in new ReservedConnection {
      given(
        ura.request(CorrelationId(0, 3), Provision(ConnectionId)),
        upa.response(CorrelationId(0, 4), ProvisionConfirmed("ConnectionIdA")))

      dataPlaneStatus.isActive() must beFalse
    }

    "have a data plane active on data plane change" in new ReservedConnection {
      given(
        ura.request(CorrelationId(0, 3), Provision(ConnectionId)),
        upa.response(CorrelationId(0, 4), ProvisionConfirmed("ConnectionIdA")))

      when(upa.notification(CorrelationId(0, 5), DataPlaneStateChange("ConnectionIdA", dataPlaneStatusType(true), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))

      dataPlaneStatus.isActive() must beTrue
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(
        agg.notification(CorrelationId(0, 6), DataPlaneStateChange(ConnectionId, dataPlaneStatusType(true), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))
    }

    "have a data plane inactive on data plane change" in new ReservedConnection {
      given(
        ura.request(CorrelationId(0, 3), Provision(ConnectionId)),
        upa.response(CorrelationId(0, 4), ProvisionConfirmed("ConnectionIdA")),
        upa.notification(CorrelationId(0, 5), DataPlaneStateChange("ConnectionIdA", dataPlaneStatusType(true), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))

      when(upa.notification(CorrelationId(0, 6), DataPlaneStateChange("ConnectionIdA", dataPlaneStatusType(false), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:15"))))

      dataPlaneStatus.isActive() must beFalse
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(
        agg.notification(CorrelationId(0, 7), DataPlaneStateChange(ConnectionId, dataPlaneStatusType(false), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:15"))))
    }
  }

  private def dataPlaneStatusType(active: Boolean) = new DataPlaneStatusType().withActive(active).withVersion(0).withVersionConsistent(true)
}
