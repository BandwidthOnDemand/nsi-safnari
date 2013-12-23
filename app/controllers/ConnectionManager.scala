package controllers

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import nl.surfnet.safnari._
import org.joda.time.DateTime
import org.joda.time.DateTimeUtils
import org.joda.time.Instant
import org.ogf.schemas.nsi._2013._07.connection.types.LifecycleStateEnumType
import org.ogf.schemas.nsi._2013._07.connection.types.NotificationBaseType
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType
import play.Logger
import scala.concurrent._
import scala.concurrent.duration.{ Duration, DurationLong }
import scala.concurrent.stm._
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Try

case class Connection(actor: ActorRef) {
  def ! (operation: Connection.Operation): Unit = actor ! operation
  def ? (operation: Connection.Operation)(implicit timeout: Timeout): Future[operation.Result] =
    (actor ? operation).mapTo(operation.resultClassTag)
}
object Connection {
  sealed trait Operation {
    type Result
    def resultClassTag: ClassTag[Result]
  }
  case object Query extends Operation {
    final type Result = (ReservationConfirmCriteriaType, QuerySummaryResultType)
    final val resultClassTag = implicitly[ClassTag[Result]]
  }
  case object QuerySegments extends Operation {
    type Result = Seq[ConnectionData]
    final val resultClassTag = implicitly[ClassTag[Result]]
  }
  case object QueryNotifications extends Operation {
    type Result = Seq[NotificationBaseType]
    final val resultClassTag = implicitly[ClassTag[Result]]
  }
  case class QueryRecursive(message: FromRequester) extends Operation {
    type Result = ToRequester
    final val resultClassTag = implicitly[ClassTag[Result]]
  }
  case class Command[+T <: Message](timestamp: Instant, message: T) extends Operation {
    type Result = NsiAcknowledgement
    final val resultClassTag = implicitly[ClassTag[Result]]
  }
  case class Replay(messages: Seq[Command[Message]]) extends Operation {
    type Result = Try[Unit]
    final val resultClassTag = implicitly[ClassTag[Result]]
  }
}

class ConnectionManager(connectionFactory: (ConnectionId, NsiProviderMessage[InitialReserve]) => (ActorRef, ConnectionEntity)) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, Connection]
  private val globalReservationIdsMap = TMap.empty[GlobalReservationId, ConnectionId]
  private val connectionsByRequesterCorrelationId = TMap.empty[(RequesterNsa, CorrelationId), Connection]

  private val childConnections = TMap.empty[ConnectionId, Connection]
  val messageStore = new MessageStore()

  def add(connectionId: ConnectionId, globalReservationId: Option[GlobalReservationId], connection: Connection) = {
    connections.single(connectionId) = connection
    globalReservationId map (globalReservationIdsMap.single(_) = connectionId)
  }

  def get(connectionId: ConnectionId): Option[Connection] = connections.single.get(connectionId)

  def find(connectionIds: Seq[ConnectionId]): Seq[Connection] = connections.single.filterKeys(connectionIds.contains).values.toSeq

  def findByGlobalReservationIds(globalReservationIds: Seq[GlobalReservationId]): Seq[Connection] = {
    val connectionIds = globalReservationIdsMap.single.filterKeys(globalReservationIds.contains).values.toSeq
    find(connectionIds)
  }

  def findByRequesterNsa(requesterNsa: RequesterNsa)(implicit executionContext: ExecutionContext): Future[Seq[Connection]] = {
    val futures = all map { actor => (actor ? Connection.Query).map(actor -> _) }
    Future.fold(futures)(List[Connection]()) {
      case (actors, (actor, (criteria, queryResult))) if (queryResult.getRequesterNSA() == requesterNsa) => actor :: actors
      case (actors, _) => actors
    }
  }

  def findByRequesterCorrelationId(requesterNsa: RequesterNsa, correlationId: CorrelationId): Option[Connection] =
    connectionsByRequesterCorrelationId.single.get((requesterNsa, correlationId))

  private def addChildConnectionId(connection: Connection, childConnectionId: ConnectionId) {
    childConnections.single(childConnectionId) = connection
  }

  def findByChildConnectionId(connectionId: ConnectionId): Option[Connection] = childConnections.single.get(connectionId)

  def all: Seq[Connection] = connections.single.values.toSeq

  def restore(implicit actorSystem: ActorSystem, executionContext: ExecutionContext): Future[Unit] = {
    val replayedConnections = Future.sequence(for {
      (connectionId, records @ (MessageRecord(_, _, _, FromRequester(NsiProviderMessage(headers, initialReserve: InitialReserve))) +: _)) <- messageStore.loadEverything()
    } yield {
      val commands = records.map { record => Connection.Command(record.createdAt, record.message) }
      val connection = createConnection(connectionId, NsiProviderMessage(headers, initialReserve))
      commands.foreach {
        case Connection.Command(_, FromRequester(message)) =>
          connectionsByRequesterCorrelationId.single.put((message.headers.requesterNSA, message.headers.correlationId), connection)
        case _ =>
      }
      (connection ? Connection.Replay(commands))
    }).map(_.collect {
      case Failure(exception) => exception
    })

    replayedConnections.flatMap { exceptions =>
      if (exceptions.isEmpty) {
        Future.successful(())
      } else {
        val exception = new Exception(s"replay failed with exceptions ${exceptions.mkString(", ")}")
        exceptions.foreach(exception.addSuppressed)
        Future.failed(exception)
      }
    }
  }

  def findOrCreateConnection(request: NsiProviderMessage[NsiProviderCommand])(implicit actorSystem: ActorSystem): Option[Connection] = atomic { implicit txn =>
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

  private def createConnection(connectionId: ConnectionId, initialReserve: NsiProviderMessage[InitialReserve])(implicit actorSystem: ActorSystem): Connection = {
    val (output, connection) = connectionFactory(connectionId, initialReserve)

    val storingActor = Connection(actorSystem.actorOf(Props(new ConnectionActor(connection, output))))
    val globalReservationId = Option(initialReserve.body.body.getGlobalReservationId()).map(URI.create)

    add(connectionId, globalReservationId, storingActor)

    storingActor
  }

  private class ConnectionActor(connection: ConnectionEntity, output: ActorRef) extends Actor {
    private val process = new IdempotentProvider(connection.aggregatorNsa, ManageChildConnections(connection.process))

    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newPassedEndTimeCorrelationId = CorrelationId.fromUuid(uuidGenerator())

    var queryRequesters: Map[CorrelationId, ActorRef] = Map.empty
    var endTimeCancellable: Option[Cancellable] = None

    import Connection._

    override def receive = LoggingReceive {
      case Query              => sender ! ((connection.rsm.criteria, connection.query))
      case QuerySegments      => sender ! connection.segments
      case QueryNotifications => sender ! connection.notifications

      case Connection.QueryRecursive(query @ FromRequester(NsiProviderMessage(_, _: QueryRecursive))) =>
        queryRequesters += (query.correlationId -> sender)

        for {
          outbounds <- connection.queryRecursive(query)
          outbound <- outbounds
        } output ! outbound

      case Command(_, inbound @ FromProvider(NsiRequesterMessage(_, _: NsiQueryRecursiveResponse))) =>
        for {
          messages <- connection.queryRecursiveResult(inbound)
          msg <- messages
          requester <- queryRequesters.get(msg.correlationId)
        } {
          queryRequesters -= msg.correlationId
          requester ! msg
        }

      case Command(timestamp, inbound: InboundMessage) =>
        val result = PersistMessages(timestamp, process)(inbound)

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
            case Command(_, inbound: InboundMessage) =>
              process(inbound).left.foreach { error =>
                Logger.warn(s"Connection ${connection.id} failed to replay message $inbound (ignored): $error")
              }
            case Command(_, outbound: OutboundMessage) =>
              connection.process(outbound)
          }

          schedulePassedEndTimeMessage()
        }

        sender ! result
    }

    private def schedulePassedEndTimeMessage(): Unit = {
      endTimeCancellable.foreach(_.cancel())
      endTimeCancellable = (for {
        lsm <- connection.lsm if lsm.lifecycleState == LifecycleStateEnumType.CREATED
        endTime <- connection.rsm.criteria.getSchedule().endTime
      } yield {
        val delay = (endTime.getMillis - DateTimeUtils.currentTimeMillis()).milliseconds
        val message = Connection.Command(endTime.toInstant, PassedEndTime(newPassedEndTimeCorrelationId, connection.id, new DateTime(endTime)))
        Logger.debug(s"Scheduling $message for execution within $delay milliseconds")
        context.system.scheduler.scheduleOnce(delay) {
          self ! message
        }(context.dispatcher)
      })
    }

    private def PersistMessages[E](timestamp: Instant, wrapped: InboundMessage => Either[E, Seq[OutboundMessage]]): InboundMessage => Either[E, Seq[OutboundMessage]] = { inbound =>
      val result = wrapped(inbound)
      result.right.foreach { outbound =>
        messageStore.storeInboundWithOutboundMessages(connection.id, timestamp, inbound, outbound)
      }
      result
    }

    private def ManageChildConnections[E, A](wrapped: InboundMessage => Either[E, A]): InboundMessage => Either[E, A] = { inbound =>
      val outbound = wrapped(inbound)
      if (outbound.isRight) {
        updateChildConnection(inbound)
      }
      outbound
    }

    private def updateChildConnection(message: InboundMessage): Unit = message match {
      case AckFromProvider(NsiProviderMessage(_, ReserveResponse(connectionId))) => addChildConnectionId(Connection(self), connectionId)
      case FromProvider(NsiRequesterMessage(_, ReserveConfirmed(connectionId, _))) => addChildConnectionId(Connection(self), connectionId)
      case FromProvider(NsiRequesterMessage(_, ReserveFailed(body))) => addChildConnectionId(Connection(self), body.getConnectionId)
      case _ =>
    }

  }

  private def messageNotApplicable(message: InboundMessage): ServiceExceptionType = NsiError.InvalidTransition.toServiceException(Configuration.Nsa)
}
