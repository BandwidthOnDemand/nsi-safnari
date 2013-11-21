package controllers

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import java.net.URI
import nl.surfnet.safnari._
import play.Logger
import scala.concurrent._
import scala.concurrent.duration.{ Duration, DurationInt }
import scala.concurrent.stm._
import scala.util.Failure
import scala.util.Try
import org.ogf.schemas.nsi._2013._07.connection.types.QuerySummaryResultType
import org.ogf.schemas.nsi._2013._07.framework.types.ServiceExceptionType

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

  def findByRequesterNsa(requesterNsa: RequesterNsa)(implicit executionContext: ExecutionContext) =
    all filter(a => Await.result((a ? 'query).mapTo[QuerySummaryResultType].map(_.getRequesterNSA().equals(requesterNsa)), 20.seconds))

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
    private val process = new IdempotentProvider(ManageChildConnections(connection.process))

    var queryRequesters: Map[CorrelationId, ActorRef] = Map.empty

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

      case inbound @ FromProvider(NsiRequesterMessage(_, _: NsiQueryRecursiveResponse)) =>
        for {
          messages <- connection.queryRecursiveResult(inbound)
          msg <- messages
          requester <- queryRequesters.get(msg.correlationId)
        } {
          queryRequesters -= msg.correlationId
          requester ! msg
        }

      case inbound: InboundMessage =>
        val result = PersistMessages(process)(inbound)

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
        }

        sender ! result
    }

    private class IdempotentProvider(wrapped: InboundMessage => Option[Seq[OutboundMessage]]) extends (InboundMessage => Either[ServiceExceptionType, (Boolean, Seq[OutboundMessage])]) {
      private var fromRequester = Map.empty[CorrelationId, (FromRequester, Option[ToRequester])]
      private var fromRequesterByDownstreamCorrelationId = Map.empty[CorrelationId, FromRequester]
      private var downstreamRequestsByFromRequesterCorrelationId = Map.empty[CorrelationId, Map[CorrelationId, OutboundMessage]].withDefaultValue(Map.empty)

      override def apply(message: InboundMessage) = message match {
        case inbound @ FromRequester(NsiProviderMessage(_, _: NsiProviderCommand)) =>
          fromRequester.get(inbound.correlationId) match {
            case None =>
              val result = wrapped(inbound)
              result.foreach(recordOutput(inbound, _))
              result.map((false, _)).toRight(messageNotApplicable(inbound))
            case Some((original, result)) =>
              if (!sameMessage(inbound.message, original.message)) {
                Left(NsiError.PayloadError.toServiceException("FIXME-NSA-ID").withText(s"duplicate request with existing correlation id ${inbound.correlationId} does not match the original"))
              } else {
                result.fold {
                  val downstreamMessages = downstreamRequestsByFromRequesterCorrelationId(inbound.correlationId)
                  Right((true, downstreamMessages.values.toSeq))
                } { reply =>
                  Right((true, Seq(reply)))
                }
              }
          }
        case inbound =>
          val originalRequest = fromRequesterByDownstreamCorrelationId.get(inbound.correlationId)
          originalRequest match {
            case None =>
              Left(NsiError.PayloadError.toServiceException("FIXME-NSA-ID").withText(s"unknown reply correlationId ${inbound.correlationId}. Expected one of ${fromRequesterByDownstreamCorrelationId.keySet}"))
            case Some(originalRequest) =>
              val result = wrapped(inbound)
              result.foreach { output =>
                recordOutput(originalRequest, output)
                downstreamRequestsByFromRequesterCorrelationId += originalRequest.correlationId -> (downstreamRequestsByFromRequesterCorrelationId(originalRequest.correlationId) - inbound.correlationId)
              }
              result.map((false, _)).toRight(messageNotApplicable(inbound))
          }
      }

      private def recordOutput(inbound: FromRequester, output: Seq[OutboundMessage]): Unit = {
        val toRequester = output.collectFirst {
          case outbound @ ToRequester(_) =>
            assert(inbound.correlationId == outbound.correlationId, s"reply correlationId ${outbound.correlationId} did not match request correlationId ${inbound.correlationId}")
            outbound
        }
        var outboundMessagesByCorrelationId = output.foldLeft(Map.empty[CorrelationId, OutboundMessage]) { (acc, message) => acc + (message.correlationId -> message) }
        output.foreach { downstream =>
          downstream match {
            case _: ToPce | _: ToProvider =>
              fromRequesterByDownstreamCorrelationId += downstream.correlationId -> inbound
              downstreamRequestsByFromRequesterCorrelationId += inbound.correlationId -> (downstreamRequestsByFromRequesterCorrelationId(inbound.correlationId) + (downstream.correlationId -> downstream))
            case _: ToRequester =>
            // Ignore
          }
        }
        fromRequester += inbound.correlationId -> (inbound -> toRequester)
      }

      private def sameMessage(a: NsiProviderMessage[NsiProviderOperation], b: NsiProviderMessage[NsiProviderOperation]): Boolean = {
        // JAXB documents cannot be compared directly due to broken equals implementation of the DOM tree.
        // Serialize both messages to XML and compare the resulting strings instead.
        import NsiSoapConversions._
        val conversion = NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(DocumentToString)
        conversion(a) == conversion(b)
      }
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

    private def messageNotApplicable(message: InboundMessage): ServiceExceptionType = NsiError.InvalidTransition.toServiceException(Configuration.Nsa)
  }
}
