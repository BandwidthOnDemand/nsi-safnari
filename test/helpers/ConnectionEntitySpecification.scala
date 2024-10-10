package helpers

import java.time.{Clock, Instant, ZoneId}
import java.time.temporal.*
import javax.xml.datatype.{DatatypeFactory, XMLGregorianCalendar}

import org.ogf.schemas.nsi._2013._12.connection.types.*
import org.specs2.execute.{Failure, FailureException}

import nl.surfnet.bod.nsi.Nillable
import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.utils.*
import nl.surfnet.safnari.*
import NsiMessages.*
import java.{util as ju}

abstract class ConnectionEntitySpecification extends helpers.Specification:

  def dataPlaneStatusType(active: Boolean, consistent: Boolean): DataPlaneStatusType =
    new DataPlaneStatusType()
      .withActive(active)
      .withVersion(ConfirmedCriteriaVersion)
      .withVersionConsistent(consistent)

  abstract class fixture extends org.specs2.matcher.Scope:
    def pathComputationAlgorithm: PathComputationAlgorithm = PathComputationAlgorithm.Chain

    val mockUuidGenerator: () => ju.UUID = Uuid.mockUuidGenerator(1)
    def newCorrelationId: CorrelationId = CorrelationId.fromUuid(mockUuidGenerator())

    val ConnectionId = "ConnectionId"
    val ReserveCorrelationId = newCorrelationId
    val CommitCorrelationId = newCorrelationId
    val AbortCorrelationId = CommitCorrelationId

    val ModifyCorrelationId: CorrelationId = helpers.Specification.newCorrelationId()
    def ModifyReserveType: ReserveType = new ReserveType()
      .withConnectionId(connection.id)
      .withCriteria(
        new ReservationRequestCriteriaType()
          .withVersion(InitialReserveType.getCriteria.getVersion + 1)
          .withSchedule(new ScheduleType().withStartTime(Nillable.absent[XMLGregorianCalendar]))
          .withModifiedCapacity(500)
      )

    val NsiReplyToUri = agg.ProviderReplyToUri
    val PceReplyToUri = agg.PceReplyToUri

    def toProviderHeaders(provider: ProviderEndPoint, correlationId: CorrelationId): NsiHeaders =
      NsiHeaders(
        correlationId,
        AggregatorNsa,
        provider.nsa,
        Some(NsiReplyToUri),
        NsiHeaders.ProviderProtocolVersion
      )

    var connection: ConnectionEntity = _
    var processInbound: IdempotentProvider = _
    var context: ConnectionContext = ConnectionContext(clock = Clock.systemDefaultZone)

    def schedule: ScheduleType = connection.rsm.committedCriteria.map(
      _.getSchedule
    ) getOrElse connection.rsm.pendingCriteria.get.getSchedule()

    def `given`(messages: Message*): Unit = messages.foreach {
      case inbound @ FromProvider(NsiRequesterMessage(_, _: QueryRecursiveConfirmed)) =>
        connection.queryRecursiveResult(inbound)
      case inbound @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) =>
        connection.queryRecursive(inbound)
      case inbound @ FromRequester(NsiProviderMessage(_, _: InitialReserve)) =>
        initialReserve(inbound)
      case inbound: InboundMessage =>
        processInbound(inbound)(
          context
        ) aka s"given message $inbound must be processed" must beRight
      case outbound: OutboundMessage =>
        connection.process(outbound)(context)
    }

    private def initialReserve(reserve: FromRequester) =
      connection = new ConnectionEntity(
        AggregatorNsa,
        ConnectionId,
        reserve.message.asInstanceOf[NsiProviderMessage[InitialReserve]],
        () => newCorrelationId,
        pathComputationAlgorithm,
        NsiReplyToUri,
        PceReplyToUri
      )

      processInbound = new IdempotentProvider(AggregatorNsa, connection.process)

      processInbound(reserve)(context).toOption

    var messages: Seq[Message] = Nil
    def when(message: InboundMessage): Option[Seq[OutboundMessage]] =
      messages = Nil

      val response = message match
        case query @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) =>
          connection.queryRecursive(query)
        case reserve @ FromRequester(NsiProviderMessage(_, _: InitialReserve))
            if connection == null =>
          initialReserve(reserve)
        case message: FromRequester =>
          val first = processInbound(message)(context).toOption
          val second = processInbound(message)(context).toOption
          second aka "idempotent retransmit" must beEqualTo(first)
          first
        case _: InboundMessage =>
          processInbound(message)(context).toOption
      response.tap(_.foreach { outbound =>
        // Validate outbound messages against XML schema.
        outbound.foreach {
          case ToRequester(msg) =>
            import nl.surfnet.nsiv2.soap.NsiSoapConversions.*
            val conversion = NsiRequesterMessageToDocument(None)(
              NsiRequesterOperationToElement
            ) andThen NsiXmlDocumentConversion
            conversion.apply(msg).get
          case ToProvider(msg, _) =>
            import nl.surfnet.nsiv2.soap.NsiSoapConversions.*
            val conversion = NsiProviderMessageToDocument(None)(
              NsiProviderOperationToElement
            ) andThen NsiXmlDocumentConversion
            conversion.apply(msg).get
          case ToPce(_) =>
          // No schema to validate against.
        }
        messages = outbound
      })
    end when

    def connectionData = connection.query
    def segments = connection.segments

    def childConnectionData(childConnectionId: ConnectionId): ConnectionData =
      segments.find(_.connectionId == Present(childConnectionId)).getOrElse {
        throw new FailureException(Failure(s"no child data for $childConnectionId"))
      }

    def reservationState: ReservationStateEnumType =
      connectionData.getConnectionStates().getReservationState()
    def provisionState: ProvisionStateEnumType =
      connectionData.getConnectionStates().getProvisionState()
    def lifecycleState: LifecycleStateEnumType =
      connectionData.getConnectionStates().getLifecycleState()
    def dataPlaneStatus: DataPlaneStatusType =
      connectionData.getConnectionStates().getDataPlaneStatus()
  end fixture

  abstract class ReservedConnection extends fixture:
    `given`(
      ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
      pce.confirm(CorrelationId(0, 1), A),
      upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
      ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)),
      upa.response(CorrelationId(0, 6), ReserveCommitConfirmed("ConnectionIdA"))
    )

  abstract class ReserveHeldConnectionWithTwoSegments extends fixture:
    `given`(
      ura.request(ReserveCorrelationId, InitialReserve(InitialReserveType)),
      pce.confirm(CorrelationId(0, 1), A, B),
      upa.acknowledge(CorrelationId(0, 4), ReserveResponse("ConnectionIdA")),
      upa.acknowledge(CorrelationId(0, 5), ReserveResponse("ConnectionIdB")),
      upa.response(CorrelationId(0, 4), ReserveConfirmed("ConnectionIdA", ConfirmCriteria)),
      upa.response(CorrelationId(0, 5), ReserveConfirmed("ConnectionIdB", ConfirmCriteria))
    )

  abstract class ReservedConnectionWithTwoSegments extends ReserveHeldConnectionWithTwoSegments:
    `given`(
      ura.request(CommitCorrelationId, ReserveCommit(ConnectionId)),
      upa.response(CorrelationId(0, 8), ReserveCommitConfirmed("ConnectionIdA")),
      upa.response(CorrelationId(0, 9), ReserveCommitConfirmed("ConnectionIdB"))
    )

  trait Modified:
    this: ReservedConnection =>
    `given`(ura.request(ModifyCorrelationId, ModifyReserve(ModifyReserveType)))

  trait Released:
    this: ReservedConnection =>

  trait ReleasedSegments:
    this: ReservedConnectionWithTwoSegments =>

  trait Provisioned:
    this: ReservedConnection =>
    val ProvisionCorrelationId = newCorrelationId

    `given`(
      ura.request(ProvisionCorrelationId, Provision(ConnectionId)),
      upa.response(CorrelationId(0, 8), ProvisionConfirmed("ConnectionIdA"))
    )

  trait DataPlaneActive:
    this: ReservedConnection =>
    `given`(
      upa.notification(
        newCorrelationId,
        DataPlaneStateChange(
          new DataPlaneStateChangeRequestType()
            .withConnectionId("ConnectionIdA")
            .withDataPlaneStatus(dataPlaneStatusType(active = true, consistent = true))
            .withTimeStamp(
              DatatypeFactory.newInstance().newXMLGregorianCalendar("2002-10-09T11:00:00Z")
            )
        )
      )
    )

  trait PassedEndTime:
    this: ReservedConnection =>
    val endTime: Instant = schedule.endTime.fold2(identity, Instant.now, Instant.now)
    context = context.copy(clock = Clock.fixed(endTime, ZoneId.systemDefault()))
    `given`(PassedEndTime(CorrelationId(3, 0), connection.id, endTime))

  trait Failed:
    this: ReservedConnection =>
    val TimeStamp: XMLGregorianCalendar =
      Instant.now().minus(3, ChronoUnit.MINUTES).toXMLGregorianCalendar()

    `given`(
      upa.notification(
        newCorrelationId,
        ErrorEvent(
          new ErrorEventType()
            .withConnectionId("ConnectionIdA")
            .withNotificationId(4)
            .withTimeStamp(TimeStamp)
            .withEvent(EventEnumType.FORCED_END)
        )
      )
    )

  trait ProvisionedSegments:
    this: ReservedConnectionWithTwoSegments =>
    val ProvisionCorrelationId = newCorrelationId

    `given`(
      ura.request(ProvisionCorrelationId, Provision(ConnectionId)),
      upa.response(CorrelationId(0, 11), ProvisionConfirmed("ConnectionIdA")),
      upa.response(CorrelationId(0, 12), ProvisionConfirmed("ConnectionIdB"))
    )
end ConnectionEntitySpecification
