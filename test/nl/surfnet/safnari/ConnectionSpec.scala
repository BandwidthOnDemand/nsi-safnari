package nl.surfnet.safnari

import java.util.UUID
import java.net.URI
import javax.xml.datatype.DatatypeFactory
import scala.collection.JavaConverters._
import org.specs2.specification.Scope
import org.ogf.schemas.nsi._2013._04.connection.types._
import org.ogf.schemas.nsi._2013._04.framework.types.ServiceExceptionType
import org.ogf.schemas.nsi._2013._04.framework.types.TypeValuePairListType

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class ConnectionSpec extends helpers.Specification {
  trait fixture extends Scope {

    val Criteria = new ReservationConfirmCriteriaType().withSchedule(new ScheduleType()).withBandwidth(100).withServiceAttributes(new ServiceAttributesType()).withPath(new PathType())
    val InitialReserveType = new ReserveType().withCriteria(Conversion.convert(Criteria).right.get)
    val A = ComputedSegment(new StpType().withLocalId("A"), new StpType().withLocalId("X"), ProviderEndPoint("urn:ogf:network:es.net", URI.create("http://example.com/provider"), NoAuthentication))
    val B = ComputedSegment(new StpType().withLocalId("X"), new StpType().withLocalId("B"), ProviderEndPoint("urn:ogf:network:surfnet.nl", URI.create("http://excample.com/provider"), NoAuthentication))

    val Headers = NsiHeaders(newCorrelationId, "RequesterNSA", "ProviderNSA", Some(URI.create("http://example.com/")))
    val ConnectionId = "ConnectionId"
    val ReserveCorrelationId = newCorrelationId
    val CommitCorrelationId = newCorrelationId

    val InitialReserveHeaders = Headers.copy(correlationId = ReserveCorrelationId)
    val InitialReserve = Reserve(InitialReserveHeaders, InitialReserveType)
    val InitialMessages = Seq(FromRequester(InitialReserve))

    val mockUuidGenerator = Uuid.mockUuidGenerator(1)
    val NsiReplyToUri = URI.create("http://example.com/nsi/requester")
    val PceReplyToUri = URI.create("http://example.com/pce/reply")
    val AggregatorNsa = "urn:ogf:network:nsa:surfnet-nsi-safnari"

    def toProviderHeaders(provider: ProviderEndPoint, correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, provider.nsa, Some(NsiReplyToUri))
    def toRequesterHeaders(correlationId: CorrelationId) = NsiHeaders(correlationId, AggregatorNsa, "RequesterNSA", None)

    val connection = new ConnectionEntity(ConnectionId, InitialReserve, () => CorrelationId.fromUuid(mockUuidGenerator()), AggregatorNsa, NsiReplyToUri, PceReplyToUri)

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
  }

  trait ReservedConnection extends fixture {
    given(InitialMessages ++ Seq(
      FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
      FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)).asReply, "ConnectionIdA", Criteria)),
      FromRequester(ReserveCommit(Headers.copy(correlationId = CommitCorrelationId), ConnectionId)),
      FromProvider(ReserveCommitConfirmed(Headers.copy(correlationId = CorrelationId(0, 3)).asReply, "ConnectionIdA"))): _*)
  }

  trait Released { this: ReservedConnection =>
  }

  trait Provisioned { this: ReservedConnection =>
    val ProvisionCorrelationId = newCorrelationId

    given(
      FromRequester(Provision(Headers.copy(correlationId = ProvisionCorrelationId), ConnectionId)),
      FromProvider(ProvisionConfirmed(Headers.copy(correlationId = newCorrelationId), "ConnectionIdA")))
  }

  "A connection" should {
    "send a path computation request when reserve is received" in new fixture {
      when(FromRequester(Reserve(Headers.copy(correlationId = ReserveCorrelationId), InitialReserveType)))

      messages must contain(ToPce(PathComputationRequest(
        correlationId = CorrelationId(0, 1),
        replyTo = PceReplyToUri,
        criteria = Criteria)))
    }

    "send reserve request for each segment when path computation confirmed is received" in new fixture {
      given(InitialMessages: _*)

      when(FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))))

      messages must haveSize(2)
      messages must haveAllElementsLike {
        case ToProvider(reserve: Reserve, A.provider) => ok
        case ToProvider(reserve: Reserve, B.provider) => ok
      }
    }

    "fail the connection when path computation fails" in new fixture {
      given(InitialMessages: _*)

      when(FromPce(PathComputationFailed(CorrelationId(0, 1), "failed")))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(headers, failed)) =>
          headers must beEqualTo(Headers.copy(correlationId = ReserveCorrelationId).asReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.PathComputationNoPath.id)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "confirm the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A)))): _*)

      when(FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)), "connectionId", Criteria)))

      messages must contain(ToRequester(ReserveConfirmed(InitialReserveHeaders.asReply, ConnectionId, Criteria)))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
    }

    "be in reservation held state when both segments are confirmed" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B)))): _*)

      when(FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)), "ConnectionIdA", Criteria)))
      messages must beEmpty

      when(FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 3)), "ConnectionIdB", Criteria)))
      messages must contain(ToRequester(ReserveConfirmed(InitialReserveHeaders.asReply, ConnectionId, Criteria)))

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_HELD)
    }

    "fail the reservation with a single path segment" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A)))): _*)

      when(FromProvider(ReserveFailed(Headers.copy(correlationId = CorrelationId(0, 2)), new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(A.provider.nsa)))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(headers, failed)) =>
          headers must beEqualTo(InitialReserveHeaders.asReply)
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
        FromProvider(ReserveFailed(Headers.copy(correlationId = CorrelationId(0, 2)), new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(A.provider.nsa))))): _*)

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 3)), "connectionIdB", Criteria)))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(headers, failed)) =>
          headers must beEqualTo(InitialReserveHeaders.asReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(1)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "fail the reservation with two segments when both fail" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A, B))),
        FromProvider(ReserveFailed(Headers.copy(correlationId = CorrelationId(0, 2)), new GenericFailedType().withConnectionId("ConnectionIdA").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(A.provider.nsa))))): _*)

      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_CHECKING)

      when(FromProvider(ReserveFailed(Headers.copy(correlationId = CorrelationId(0, 3)), new GenericFailedType().withConnectionId("ConnectionIdB").withServiceException(NsiError.BandwidthNotAvailable.toServiceException(B.provider.nsa)))))

      messages must haveSize(1)
      messages must haveOneElementLike {
        case ToRequester(ReserveFailed(headers, failed)) =>
          headers must beEqualTo(InitialReserveHeaders.asReply)
          failed.getConnectionId() must beEqualTo(ConnectionId)
          failed.getServiceException().getErrorId() must beEqualTo(NsiError.ChildError.id)
          failed.getServiceException().getChildException().asScala must haveSize(2)
      }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_FAILED)
    }

    "be in committing state when reserve commit is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)), ConnectionId, Criteria))): _*)

      when(FromRequester(ReserveCommit(Headers.copy(correlationId = newCorrelationId), ConnectionId)))

      messages must haveOneElementLike { case ToProvider(_: ReserveCommit, _) => ok }
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_COMMITTING)
    }

    "be in reserved state when reserve commit confirmed is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)), "ConnectionIdA", Criteria)),
        FromRequester(ReserveCommit(Headers.copy(correlationId = CommitCorrelationId), ConnectionId))): _*)

      when(FromProvider(ReserveCommitConfirmed(Headers.copy(correlationId = CorrelationId(0, 3)), "ConnectionIdA")))

      messages must contain(ToRequester(ReserveCommitConfirmed(Headers.copy(correlationId = CommitCorrelationId).asReply, ConnectionId)))
      reservationState must beEqualTo(ReservationStateEnumType.RESERVE_START)
    }

    "reject commit when in initial state" in new fixture {
      val ack = when(FromRequester(ReserveCommit(Headers.copy(correlationId = CommitCorrelationId), ConnectionId)))

      ack must beNone
    }

    "be in aborting state when reserve abort is received" in new fixture {
      given(InitialMessages ++ Seq(
        FromPce(PathComputationConfirmed(CorrelationId(0, 1), Seq(A))),
        FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)), "ConnectionIdA", Criteria))): _*)

      when(FromRequester(ReserveAbort(Headers.copy(correlationId = ReserveCorrelationId), ConnectionId)))

      messages must haveOneElementLike { case ToProvider(_: ReserveAbort, _) => ok }
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

      when(FromProvider(ReserveConfirmed(Headers.copy(correlationId = CorrelationId(0, 2)), "ConnectionIdA", Criteria)))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
    }

    "become provisioning on provision request" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      when(FromRequester(Provision(Headers.copy(correlationId = ProvisionCorrelationId), ConnectionId)))

      messages must contain(ToProvider(Provision(toProviderHeaders(A.provider, CorrelationId(0, 5)), "ConnectionIdA"), A.provider))
    }

    "send a provision confirmed to requester" in new ReservedConnection with Released {
      val ProvisionCorrelationId = newCorrelationId

      given(FromRequester(Provision(Headers.copy(correlationId = ProvisionCorrelationId), ConnectionId)))

      when(FromProvider(ProvisionConfirmed(Headers.copy(correlationId = CorrelationId(0, 4)), "ConnectionIdA")))

      provisionState must beEqualTo(ProvisionStateEnumType.PROVISIONED)
      messages must contain(ToRequester(ProvisionConfirmed(Headers.copy(correlationId = ProvisionCorrelationId).asReply, ConnectionId)))
    }

    "become releasing on release request" in new ReservedConnection with Provisioned {
      val ReleaseCorrelationId = newCorrelationId

      when(FromRequester(Release(Headers.copy(correlationId = ReleaseCorrelationId), ConnectionId)))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASING)
      messages must contain(ToProvider(Release(toProviderHeaders(A.provider, CorrelationId(0, 6)), "ConnectionIdA"), A.provider))
    }

    "send release confirmed to requester" in new ReservedConnection with Provisioned {
      val ReleaseCorrelationId = newCorrelationId

      given(FromRequester(Release(Headers.copy(correlationId = ReleaseCorrelationId), ConnectionId)))

      when(FromProvider(ReleaseConfirmed(Headers.copy(correlationId = CorrelationId(0, 5)), "ConnectionIdA")))

      provisionState must beEqualTo(ProvisionStateEnumType.RELEASED)
      messages must contain(ToRequester(ReleaseConfirmed(Headers.copy(correlationId = ReleaseCorrelationId).asReply, ConnectionId)))
    }

    "reject release request when released" in new ReservedConnection with Released {
      val ack = when(FromRequester(Release(Headers.copy(correlationId = newCorrelationId), ConnectionId)))

      ack must beNone
    }

    "reject provision request when provisioned" in new ReservedConnection with Provisioned {
      val ack = when(FromRequester(Provision(Headers.copy(correlationId = newCorrelationId), ConnectionId)))

      ack must beNone
    }

    "become terminating on terminate request" in new ReservedConnection {
      val TerminateCorrelationId = newCorrelationId

      when(FromRequester(Terminate(Headers.copy(correlationId = TerminateCorrelationId), ConnectionId)))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATING)
      messages must contain(ToProvider(Terminate(toProviderHeaders(A.provider, CorrelationId(0, 5)), "ConnectionIdA"), A.provider))
    }

    "send a terminate confirmed to requester" in new ReservedConnection {
      val TerminateCorrelationId = newCorrelationId

      given(FromRequester(Terminate(Headers.copy(correlationId = TerminateCorrelationId), ConnectionId)))

      when(FromProvider(TerminateConfirmed(Headers.copy(correlationId = CorrelationId(0, 4)), "ConnectionIdA")))

      lifecycleState must beEqualTo(LifecycleStateEnumType.TERMINATED)
      messages must contain(ToRequester(TerminateConfirmed(Headers.copy(correlationId = TerminateCorrelationId).asReply, ConnectionId)))
    }

    "have a data plane inactive" in new ReservedConnection {
      given(
        FromRequester(Provision(Headers.copy(correlationId = CorrelationId(0, 3)), ConnectionId)),
        FromProvider(ProvisionConfirmed(Headers.copy(correlationId = CorrelationId(0, 4)), "ConnectionIdA")))

      dataPlaneStatus.isActive() must beFalse
    }

    "have a data plane active on data plane change" in new ReservedConnection {
      given(
        FromRequester(Provision(Headers.copy(correlationId = CorrelationId(0, 3)), ConnectionId)),
        FromProvider(ProvisionConfirmed(Headers.copy(correlationId = CorrelationId(0, 4)), "ConnectionIdA")))

      when(FromProvider(DataPlaneStateChange(Headers.copy(correlationId = CorrelationId(0, 5)), "ConnectionIdA", dataPlaneStatusType(true), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))

      dataPlaneStatus.isActive() must beTrue
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(ToRequester(DataPlaneStateChange(toRequesterHeaders(CorrelationId(0, 6)), ConnectionId, dataPlaneStatusType(true), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))
    }

    "have a data plane inactive on data plane change" in new ReservedConnection {
      given(
        FromRequester(Provision(Headers.copy(correlationId = CorrelationId(0, 3)), ConnectionId)),
        FromProvider(ProvisionConfirmed(Headers.copy(correlationId = CorrelationId(0, 4)), "ConnectionIdA")),
        FromProvider(DataPlaneStateChange(Headers.copy(correlationId = CorrelationId(0, 5)), "ConnectionIdA", dataPlaneStatusType(true), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))

      when(FromProvider(DataPlaneStateChange(Headers.copy(correlationId = CorrelationId(0, 6)), "ConnectionIdA", dataPlaneStatusType(false), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))

      dataPlaneStatus.isActive() must beFalse
      dataPlaneStatus.isVersionConsistent() must beTrue
      messages must contain(ToRequester(DataPlaneStateChange(toRequesterHeaders(CorrelationId(0, 7)), ConnectionId, dataPlaneStatusType(false), DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09-11:00"))))
    }
  }

  private def dataPlaneStatusType(active: Boolean) = new DataPlaneStatusType().withActive(active).withVersion(0).withVersionConsistent(true)

}
