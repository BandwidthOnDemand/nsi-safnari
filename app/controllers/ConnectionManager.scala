package controllers

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import nl.surfnet.safnari._
import play.Logger
import scala.concurrent._
import scala.concurrent.duration.{ Duration, DurationLong }
import scala.concurrent.stm._
import scala.util.Failure
import scala.util.Try
import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import org.joda.time.DateTime
import org.joda.time.DateTimeUtils

class ConnectionManager(connectionFactory: (ConnectionId, NsiProviderMessage[InitialReserve]) => (ActorRef, ConnectionEntity)) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]
  private val globalReservationIdsMap = TMap.empty[GlobalReservationId, ConnectionId]
  private val connectionsByRequesterCorrelationId = TMap.empty[(RequesterNsa, CorrelationId), ActorRef]

  private val childConnections = TMap.empty[ConnectionId, ActorRef]
  private val messageStore = new MessageStore[Message]()

  def add(connectionId: ConnectionId, globalReservationId: Option[GlobalReservationId], connection: ActorRef) = {
    connections.single(connectionId) = connection
    globalReservationId map (globalReservationIdsMap.single(_) = connectionId)
  }

  def get(connectionId: ConnectionId): Option[ActorRef] = connections.single.get(connectionId)

  def find(connectionIds: Seq[ConnectionId]): Seq[ActorRef] = connections.single.filterKeys(connectionIds.contains).values.toSeq

  def findByGlobalReservationIds(globalReservationIds: Seq[GlobalReservationId]): Seq[ActorRef] = {
    val connectionIds = globalReservationIdsMap.single.filterKeys(globalReservationIds.contains).values.toSeq
    find(connectionIds)
  }

  def findByRequesterNsa(requesterNsa: RequesterNsa)(implicit executionContext: ExecutionContext): Future[Seq[ActorRef]] = {
    val futures = all map { actor => (actor ? 'query).mapTo[QuerySummaryResultType].map(actor -> _) }
    Future.fold(futures)(List[ActorRef]()) {
      case (actors, (actor, queryResult)) if (queryResult.getRequesterNSA() == requesterNsa) => actor :: actors
      case (actors, _) => actors
    }
  }

  def findByRequesterCorrelationId(requesterNsa: RequesterNsa, correlationId: CorrelationId): Option[ActorRef] =
    connectionsByRequesterCorrelationId.single.get((requesterNsa, correlationId))

  private def addChildConnectionId(connection: ActorRef, childConnectionId: ConnectionId) {
    childConnections.single(childConnectionId) = connection
  }

  def findByChildConnectionId(connectionId: ConnectionId): Option[ActorRef] = childConnections.single.get(connectionId)

  def all: Seq[ActorRef] = connections.single.values.toSeq

  def restore(implicit actorSystem: ActorSystem, executionContext: ExecutionContext): Future[Unit] = {
    val replayedConnections = Future.sequence(for {
      (connectionId, messages @ (FromRequester(NsiProviderMessage(headers, initialReserve: InitialReserve)) +: _)) <- messageStore.loadEverything()
    } yield {
      val connection = createConnection(connectionId, NsiProviderMessage(headers, initialReserve))
      (connection ? Replay(messages)).mapTo[Try[Unit]]
    }).map(_.collect {
      case Failure(exception) => exception
    })

    replayedConnections.flatMap { exceptions =>
      if (exceptions.isEmpty) {
        Future.successful(())
      } else {
        val exception = new Exception("replay failed")
        exceptions.foreach(exception.addSuppressed)
        Future.failed(exception)
      }
    }
  }

  def findOrCreateConnection(request: NsiProviderMessage[NsiProviderCommand])(implicit actorSystem: ActorSystem): Option[ActorRef] = atomic { implicit txn =>
    findByRequesterCorrelationId(request.headers.requesterNSA, request.headers.correlationId).orElse {
      val result = request match {
        case NsiProviderMessage(headers, update: NsiProviderUpdateCommand) =>
          get(update.connectionId)
        case NsiProviderMessage(headers, initialReserve: InitialReserve) =>
          val connectionId = newConnectionId
          val connection = createConnection(connectionId, NsiProviderMessage(headers, initialReserve))
          Some(connection)
      }
      result.foreach { connection =>
        connectionsByRequesterCorrelationId.put((request.headers.requesterNSA, request.headers.correlationId), connection)
      }
      result
    }
  }

  private def createConnection(connectionId: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve])(implicit actorSystem: ActorSystem): ActorRef = {
    val (output, connection) = connectionFactory(connectionId, initialReserve)

    val storingActor = actorSystem.actorOf(Props(new ConnectionActor(connection, output)))
    val globalReservationId = Option(initialReserve.body.body.getGlobalReservationId()).map(URI.create)

    add(connectionId, globalReservationId, storingActor)

    storingActor
  }

  private[controllers] case class Replay(messages: Seq[Message])

  private class ConnectionActor(connection: ConnectionEntity, output: ActorRef) extends Actor {
    private val process = new IdempotentProvider(connection.aggregatorNsa, ManageChildConnections(connection.process))

    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newPassedEndTimeCorrelationId = CorrelationId.fromUuid(uuidGenerator())

    var queryRequesters: Map[CorrelationId, ActorRef] = Map.empty
    var endTimeCancellable: Option[Cancellable] = None

    override def receive = LoggingReceive {
      case 'query              => sender ! connection.query
      case 'querySegments      => sender ! connection.segments
      case 'queryNotifications => sender ! connection.notifications

      case query @ FromRequester(NsiProviderMessage(_, _: QueryRecursive)) =>
        queryRequesters += (query.correlationId -> sender)
        for {
          outbounds <- connection.queryRecursive(query)
          outbound <- outbounds
        } output ! outbound

      case queryRecursiveResponse @ FromProvider(NsiRequesterMessage(_, _: QueryRecursiveConfirmed)) =>
        receivedQueryRecursiveResponse(queryRecursiveResponse)

      case queryRecursiveResponse @ FromProvider(NsiRequesterMessage(_, _: Error)) =>
        // the only error we can receive as a requester is a response to a query recursive request
        receivedQueryRecursiveResponse(queryRecursiveResponse)

      case inbound: InboundMessage =>
        val result = PersistMessages(process)(inbound)

        schedulePassedEndTimeMessage()

        val response = result match {
          case Left(error) =>
            ServiceException(error)
          case Right(outbound) =>
            outbound.foreach(connection.process)

            outbound.foreach(output ! _)

            inbound match {
              case FromRequester(NsiProviderMessage(_, _: InitialReserve)) => ReserveResponse(connection.id)
              case _                                                       => GenericAck()
            }
        }

        sender ! response

      case Replay(messages) =>
        Logger.info(s"Replaying ${messages.size} messages for connection ${connection.id}")

        val result = Try {
          messages.foreach {
            case inbound: InboundMessage =>
              process(inbound).left.foreach { error =>
                Logger.warn(s"Connection ${connection.id} failed to replay message $inbound (ignored): $error")
              }
            case outbound: OutboundMessage =>
              connection.process(outbound)
          }

          schedulePassedEndTimeMessage()
        }

        sender ! result
    }

    private def receivedQueryRecursiveResponse(inbound: FromProvider): Unit =
      for {
        messages <- connection.queryRecursiveResult(inbound)
        msg <- messages
        requester <- queryRequesters.get(msg.correlationId)
      } {
        queryRequesters -= msg.correlationId
        requester ! msg
      }

    private def schedulePassedEndTimeMessage(): Unit = {
      endTimeCancellable.foreach(_.cancel())
      endTimeCancellable = (for {
        lsm <- connection.lsm if lsm.lifecycleState == LifecycleStateEnumType.CREATED
        endTime <- connection.rsm.criteria.getSchedule().endTime
      } yield {
        val delay = (endTime.getMillis - DateTimeUtils.currentTimeMillis()).milliseconds
        val message = PassedEndTime(newPassedEndTimeCorrelationId, connection.id, new DateTime(endTime))
        context.system.scheduler.scheduleOnce(delay) {
          self ! message
        }(context.dispatcher)
      })
    }

    private def PersistMessages[E](wrapped: InboundMessage => Either[E, (Boolean, Seq[OutboundMessage])]): InboundMessage => Either[E, Seq[OutboundMessage]] = { inbound =>
      val result = wrapped(inbound)
      result.right.foreach {
        case (replayed, outbound) =>
          if (!replayed) {
            messageStore.storeInboundWithOutboundMessages(connection.id, inbound, outbound)
          }
      }
      result.right.map(_._2)
    }

    private def ManageChildConnections[E, A](wrapped: InboundMessage => Option[A]): InboundMessage => Option[A] = { inbound =>
      val outbound = wrapped(inbound)
      if (outbound.isDefined) {
        updateChildConnection(inbound)
      }
      outbound
    }

    private def updateChildConnection(message: InboundMessage): Unit = message match {
      case AckFromProvider(NsiProviderMessage(_, ReserveResponse(connectionId))) => addChildConnectionId(self, connectionId)
      case FromProvider(NsiRequesterMessage(_, ReserveConfirmed(connectionId, _))) => addChildConnectionId(self, connectionId)
      case FromProvider(NsiRequesterMessage(_, ReserveFailed(body))) => addChildConnectionId(self, body.getConnectionId)
      case _ =>
    }

  }

  private def messageNotApplicable(message: InboundMessage): ServiceExceptionType = NsiError.InvalidTransition.toServiceException(Configuration.Nsa)
}
