/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package controllers

import akka.actor.*
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import java.time.{Clock, Instant, ZoneOffset}
import java.time.temporal.*
import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.persistence.*
import nl.surfnet.nsiv2.utils.*
import nl.surfnet.safnari.*
import org.ogf.schemas.nsi._2013._12.connection.types.*
import play.api.Logger
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.stm.*
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

import controllers.ActorSupport.*

case class Connection(actor: ActorRef) {
  def !(operation: Connection.Operation)(implicit sender: ActorRef): Unit = actor ! operation
  def ?(operation: Connection.Operation)(implicit timeout: Timeout): Future[operation.Result] =
    (actor ? operation).mapTo(operation.resultClassTag)
}
object Connection {
  sealed trait Operation {
    type Result
    def resultClassTag: ClassTag[Result]
  }
  case object Query extends Operation {
    final case class Result(
        summary: QuerySummaryResultType,
        pendingCriteria: Option[ReservationRequestCriteriaType],
        lastModifiedAt: Instant
    )
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case object QuerySegments extends Operation {
    type Result = Seq[ConnectionData]
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case object QueryNotifications extends Operation {
    type Result = Seq[NotificationBaseType]
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case class QueryRecursive(message: FromRequester) extends Operation {
    type Result = ToRequester
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case object QueryResults extends Operation {
    type Result = Seq[QueryResultResponseType]
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case class Command[+T <: Message](timestamp: Instant, message: T) extends Operation {
    type Result = NsiAcknowledgement
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case class Replay(messages: Seq[Command[Message]]) extends Operation {
    type Result = Try[Unit]
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Result]]
  }
  case object Delete extends Operation {
    type Result = Void
    final val resultClassTag: ClassTag[Result] = implicitly[ClassTag[Void]]
  }
}

class ConnectionManager(
    connectionFactory: (
        ConnectionId,
        NsiProviderMessage[InitialReserve]
    ) => (ActorRef, ConnectionEntity),
    configuration: Configuration,
    val messageStore: MessageStore[Message]
) {
  private val logger = Logger(classOf[ConnectionManager])

  private val connections = TMap.empty[ConnectionId, Connection]
  private val globalReservationIdsMap = TMap.empty[GlobalReservationId, Set[Connection]]
  private val connectionsByRequesterCorrelationId =
    TMap.empty[(RequesterNsa, CorrelationId), Connection]
  private val childConnections = TMap.empty[ConnectionId, Connection]
  private val deleteHooks = TMap.empty[ConnectionId, InTxn => Unit]

  private def addDeleteHook(
      connectionId: ConnectionId
  )(f: InTxn => Unit)(implicit tx: InTxn): Unit = {
    val existing = deleteHooks.getOrElse(connectionId, (_: InTxn) => ())
    deleteHooks.put(
      connectionId,
      { txn =>
        existing(txn); f(txn)
      }
    )
    ()
  }

  private def runDeleteHook(connectionId: ConnectionId): Unit = atomic { implicit txn =>
    deleteHooks.remove(connectionId).foreach { hook => hook(txn) }
  }

  def add(
      connectionId: ConnectionId,
      globalReservationId: Option[GlobalReservationId],
      connection: Connection
  ): Unit = atomic { implicit txn =>
    connections(connectionId) = connection
    addDeleteHook(connectionId) { implicit txn =>
      connections.remove(connectionId); ()
    }
    globalReservationId foreach { globalReservationId =>
      globalReservationIdsMap(globalReservationId) =
        globalReservationIdsMap.getOrElse(globalReservationId, Set()) + connection
      addDeleteHook(connectionId) { implicit txn =>
        deleteConnectionFromGlobalReservationIds(connection, globalReservationId)
      }
    }
  }

  private def deleteConnectionFromGlobalReservationIds(
      connection: Connection,
      globalReservationId: GlobalReservationId
  )(implicit txn: InTxn): Unit = {
    val remainingConnections =
      globalReservationIdsMap.getOrElse(globalReservationId, Set()) - connection
    remainingConnections match {
      case s if s.nonEmpty => globalReservationIdsMap(globalReservationId) = s
      case _               => globalReservationIdsMap.remove(globalReservationId); ()
    }
  }

  def get(connectionId: ConnectionId): Option[Connection] = connections.single.get(connectionId)

  def find(connectionIds: Seq[ConnectionId]): Seq[Connection] = {
    val connectionsMap = connections.single.snapshot
    connectionIds.flatMap(connectionsMap.get)
  }

  def findByGlobalReservationIds(
      globalReservationIds: Seq[GlobalReservationId]
  ): Seq[Connection] = {
    val globalIdsMap = globalReservationIdsMap.single.snapshot
    globalReservationIds.flatMap(globalIdsMap.getOrElse(_, Set()))
  }

  private def findByRequesterCorrelationId(
      requesterNsa: RequesterNsa,
      correlationId: CorrelationId
  ): Option[Connection] =
    connectionsByRequesterCorrelationId.single.get((requesterNsa, correlationId))

  private def addChildConnectionId(
      connection: Connection,
      aggregatedConnectionId: ConnectionId,
      childConnectionId: ConnectionId
  )(implicit txn: InTxn): Unit = {
    childConnections(childConnectionId) = connection
    addDeleteHook(aggregatedConnectionId) { implicit txn =>
      childConnections.remove(childConnectionId)
      ()
    }
  }

  private def registerRequesterAndCorrelationId(
      requesterNsa: RequesterNsa,
      correlationId: CorrelationId,
      connectionId: ConnectionId,
      connection: Connection
  ): Unit = atomic { implicit txn =>
    val key = (requesterNsa, correlationId)
    connectionsByRequesterCorrelationId(key) = connection
    addDeleteHook(connectionId) { implicit txn =>
      connectionsByRequesterCorrelationId.remove(key); ()
    }
  }

  def findByChildConnectionId(connectionId: ConnectionId): Option[Connection] =
    childConnections.single.get(connectionId)

  def all: Seq[Connection] = connections.single.values.toSeq

  def restore(implicit
      actorSystem: ActorSystem,
      executionContext: ExecutionContext
  ): Future[Unit] = {
    val replayedConnections = Future
      .sequence(for {
        case (
          connectionId,
          records @ (MessageRecord(
            _,
            _,
            _,
            FromRequester(NsiProviderMessage(headers, initialReserve: InitialReserve))
          ) +: _)
        ) <- messageStore.loadEverything()
      } yield {
        val commands = records.map { record =>
          Connection.Command(record.createdAt, record.message)
        }
        val connection =
          createConnectionActor(connectionId, NsiProviderMessage(headers, initialReserve))
        commands.foreach {
          case Connection.Command(_, FromRequester(message)) =>
            registerRequesterAndCorrelationId(
              message.headers.requesterNSA,
              message.headers.correlationId,
              connectionId,
              connection
            )
          case _ =>
        }
        (connection ? Connection.Replay(commands))
      })
      .map(_.collect { case Failure(exception) =>
        exception
      })

    replayedConnections.flatMap { exceptions =>
      if exceptions.isEmpty then {
        Future.successful(())
      } else {
        val exception = new Exception(s"replay failed with exceptions ${exceptions.mkString(", ")}")
        exceptions.foreach(exception.addSuppressed)
        Future.failed(exception)
      }
    }
  }

  def findOrCreateConnection(
      request: NsiProviderMessage[NsiProviderCommand]
  )(implicit actorSystem: ActorSystem): Option[Connection] = {
    findByRequesterCorrelationId(request.headers.requesterNSA, request.headers.correlationId)
      .orElse {
        request match {
          case NsiProviderMessage(headers @ _, update: NsiProviderUpdateCommand) =>
            get(update.connectionId)
          case NsiProviderMessage(headers, initialReserve: InitialReserve) =>
            Some(createConnection(newConnectionId(), NsiProviderMessage(headers, initialReserve)))
        }
      }
  }

  def createConnection(connectionId: ConnectionId, request: NsiProviderMessage[InitialReserve])(
      implicit actorSystem: ActorSystem
  ): Connection = {
    val connection =
      createConnectionActor(connectionId, NsiProviderMessage(request.headers, request.body))
    messageStore.create(connectionId, Instant.now(), request.headers.requesterNSA)
    registerRequesterAndCorrelationId(
      request.headers.requesterNSA,
      request.headers.correlationId,
      connectionId,
      connection
    )
    connection
  }

  private def createConnectionActor(
      connectionId: ConnectionId,
      initialReserve: NsiProviderMessage[InitialReserve]
  )(implicit actorSystem: ActorSystem): Connection = {
    val (outputActor, connectionEntity) = connectionFactory(connectionId, initialReserve)

    val connection = Connection(
      actorSystem.actorOf(
        ConnectionActor.props(connectionEntity, outputActor),
        s"con-$connectionId"
      )
    )
    val globalReservationId =
      Option(initialReserve.body.body.getGlobalReservationId()).map(URI.create)

    add(connectionId, globalReservationId, connection)

    connection
  }

  private object ConnectionActor {
    def props(connectionEntity: ConnectionEntity, outputActor: ActorRef): Props = Props(
      new ConnectionActor(connectionEntity, outputActor)
    )
  }
  private class ConnectionActor(connection: ConnectionEntity, output: ActorRef) extends Actor {
    private val process =
      new IdempotentProvider(connection.aggregatorNsa, ManageChildConnections(connection.process))

    private val uuidGenerator = Uuid.randomUuidGenerator()
    private def newPassedEndTimeCorrelationId = CorrelationId.fromUuid(uuidGenerator())

    var queryRequesters: Map[CorrelationId, ActorRef] = Map.empty
    var endTimeCancellable: Option[Cancellable] = None
    var expirationCancellable: Option[Cancellable] = None

    override def postStop(): Unit = {
      endTimeCancellable.foreach(_.cancel())
      expirationCancellable.foreach(_.cancel())
    }

    override def receive: Receive = LoggingReceive {
      case Connection.Query =>
        sender() ! Connection.Query.Result(
          connection.query,
          connection.rsm.pendingCriteria,
          connection.lastUpdatedAt
        )
      case Connection.QuerySegments      => sender() ! connection.segments
      case Connection.QueryNotifications => sender() ! connection.notifications
      case Connection.QueryResults       => sender() ! connection.results

      case Connection.QueryRecursive(
            query @ FromRequester(NsiProviderMessage(_, QueryRecursive(_, _)))
          ) =>
        queryRequesters += (query.correlationId -> sender())

        for {
          outbounds <- connection.queryRecursive(query)
          outbound <- outbounds
        } output ! outbound

      case Connection.Command(
            _,
            inbound @ FromProvider(NsiRequesterMessage(_, QueryRecursiveConfirmed(_)))
          ) =>
        for {
          messages <- connection.queryRecursiveResult(inbound)
          msg <- messages
          requester <- queryRequesters.get(msg.correlationId)
        } {
          queryRequesters -= msg.correlationId
          requester ! msg
        }

      case Connection.Command(timestamp, inbound: InboundMessage) =>
        val context = ConnectionContext(clock = Clock.fixed(timestamp, ZoneOffset.UTC))
        val result = PersistMessages(timestamp, process.apply)(inbound)(context)

        schedulePassedEndTimeMessage()

        val response: NsiAcknowledgement = result match {
          case Left(error) =>
            ServiceException(error)
          case Right(outbound) =>
            scheduleExpiration(timestamp)

            outbound.foreach(connection.process)

            outbound.foreach(output ! _)

            inbound match {
              case FromRequester(NsiProviderMessage(_, _: Reserve)) =>
                ReserveResponse(connection.id)
              case _ => GenericAck()
            }
        }

        sender() ! response

      case Connection.Replay(messages) =>
        logger.info(s"Replaying ${messages.size} messages for connection ${connection.id}")

        val result = Try {
          messages.foreach {
            case Connection.Command(timestamp, inbound: InboundMessage) =>
              val context = ConnectionContext(clock = Clock.fixed(timestamp, ZoneOffset.UTC))
              process(inbound)(context).left.foreach { error =>
                logger.warn(
                  s"Connection ${connection.id} failed to replay message $inbound (ignored): $error"
                )
              }
            case Connection.Command(timestamp, outbound: OutboundMessage) =>
              val context = ConnectionContext(clock = Clock.fixed(timestamp, ZoneOffset.UTC))
              connection.process(outbound)(context)
          }

          schedulePassedEndTimeMessage()
          messages.lastOption.foreach { lastMessage => scheduleExpiration(lastMessage.timestamp) }
        }

        sender() ! result

      case Connection.Delete =>
        logger.info(s"Stopping $connection.id")
        messageStore.delete(connection.id, Instant.now())
        runDeleteHook(connection.id)
        self ! PoisonPill
    }

    private def schedulePassedEndTimeMessage(): Unit = {
      endTimeCancellable.foreach(_.cancel())
      endTimeCancellable = (for {
        criteria <- connection.rsm.committedCriteria
        endTime <- criteria.getSchedule().endTime.toOption(None)
        if connection.lsm.lifecycleState == LifecycleStateEnumType.CREATED
      } yield {
        val delay = (endTime.toEpochMilli - Instant.now().toEpochMilli).milliseconds
        val message = Connection.Command(
          endTime,
          PassedEndTime(newPassedEndTimeCorrelationId, connection.id, endTime)
        )
        logger.debug(s"Scheduling $message for execution after $delay")
        try {
          context.system.scheduler.scheduleOnce(delay) {
            logger.debug(s"Sending scheduled message $message")
            self ! message
          }(context.dispatcher)
        } catch {
          case _: IllegalArgumentException =>
            // Akka's scheduled currently limits delays to 248 days or less. Retry scheduling later when the real delay fails.
            context.system.scheduler.scheduleOnce(100.days) {
              schedulePassedEndTimeMessage()
            }(context.dispatcher)
        }
      })
    }

    private def scheduleExpiration(lastMessageTimestamp: Instant): Unit = {
      expirationCancellable.foreach(_.cancel())
      expirationCancellable =
        if connection.psm.isDefined && connection.lsm.lifecycleState == LifecycleStateEnumType.CREATED
        then None
        else {
          val expirationTime = lastMessageTimestamp.plus(
            configuration.ConnectionExpirationTime.toMillis,
            ChronoUnit.MILLIS
          )
          val delay = (expirationTime.toEpochMilli - Instant.now().toEpochMilli).milliseconds
          val message = Connection.Delete
          logger.debug(s"Scheduling $message for execution after $delay")
          Some(context.system.scheduler.scheduleOnce(delay) {
            self ! message
          }(context.dispatcher))
        }
    }

    private def PersistMessages[E](
        timestamp: Instant,
        wrapped: InboundMessage => ConnectionContext => Either[E, Seq[OutboundMessage]]
    )(inbound: InboundMessage)(context: ConnectionContext): Either[E, Seq[OutboundMessage]] = {
      val result = wrapped(inbound)(context)
      result.foreach { outbound =>
        messageStore.storeInboundWithOutboundMessages(connection.id, timestamp, inbound, outbound)
      }
      result
    }

    private def ManageChildConnections[E, A](
        wrapped: InboundMessage => ConnectionContext => Either[E, A]
    )(inbound: InboundMessage)(context: ConnectionContext): Either[E, A] = {
      val outbound = wrapped(inbound)(context)
      if outbound.isRight then {
        updateChildConnection(inbound)
      }
      outbound
    }

    private def updateChildConnection(message: InboundMessage): Unit = atomic { implicit txn =>
      val childConnectionId = message match {
        case AckFromProvider(NsiProviderMessage(_, ReserveResponse(connectionId))) =>
          Some(connectionId)
        case FromProvider(NsiRequesterMessage(_, ReserveConfirmed(connectionId, _))) =>
          Some(connectionId)
        case FromProvider(NsiRequesterMessage(_, ReserveFailed(body))) =>
          Some(body.getConnectionId)
        case _ =>
          None
      }
      childConnectionId.foreach(addChildConnectionId(Connection(self), connection.id, _))
    }
  }
}
