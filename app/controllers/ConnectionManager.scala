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

class ConnectionManager(connectionFactory: (ConnectionId, NsiProviderMessage[InitialReserve]) => (ActorRef, ConnectionEntity)) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]
  private val globalReservationIdsMap = TMap.empty[GlobalReservationId, ConnectionId]

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

  def findByRequesterNsa(requesterNsa: String)(implicit executionContext: ExecutionContext) =
    all filter(a => Await.result((a ? 'query).mapTo[QuerySummaryResultType].map(_.getRequesterNSA().equals(requesterNsa)), 20.seconds))

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

  def findOrCreateConnection(request: NsiProviderMessage[NsiProviderOperation])(implicit actorSystem: ActorSystem): Option[ActorRef] = request match {
    case NsiProviderMessage(_, update: NsiProviderUpdateCommand) =>
      get(update.connectionId)
    case NsiProviderMessage(headers, initialReserve: InitialReserve) =>
      val connectionId = newConnectionId
      val connection = createConnection(connectionId, NsiProviderMessage(headers, initialReserve))
      Some(connection)
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
        val result = IdempotentProvider(PersistMessages(connection.process))(inbound)

        val response = result match {
          case Left(error) =>
            error
          case Right(outbound) =>
            updateChildConnection(inbound)
            outbound.foreach(connection.process)

            outbound.foreach(output ! _)

            inbound match {
              case FromRequester(NsiProviderMessage(_, _: InitialReserve)) => ReserveResponse(connection.id)
              case FromRequester(_) | FromProvider(_)                      => GenericAck()
              case FromPce(_)                                              => 200
              case AckFromProvider(_) | _: MessageDeliveryFailure          => ()
            }
        }

        sender ! response

      case Replay(messages) =>
        Logger.info(s"Replaying ${messages.size} messages for connection ${connection.id}")

        val result = Try {
          messages.foreach {
            case inbound: InboundMessage =>
              updateChildConnection(inbound)
              connection.process(inbound).getOrElse {
                Logger.warn(s"Connection ${connection.id} failed to replay message $inbound")
              }
            case outbound: OutboundMessage =>
              connection.process(outbound)
          }
        }

        sender ! result
    }

    private def IdempotentProvider(wrapped: InboundMessage => Option[Seq[OutboundMessage]]): InboundMessage => Either[Any, Seq[OutboundMessage]] = {
      case inbound @ FromRequester(NsiProviderMessage(headers, command: NsiProviderCommand)) =>
        val originalMessages = messageStore.findByCorrelationId(headers.correlationId)
        val originalRequest = originalMessages.collectFirst {
          case request @ FromRequester(_) => request
        }

        originalRequest.fold {
          wrapped(inbound).toRight(messageNotApplicable(inbound))
        } { request =>
          if (!sameMessage(inbound.message, request.message)) {
            Left(NsiError.PayloadError.toServiceException("FIXME-NSA-ID").withText(s"request with existing correlation id ${headers.correlationId} does not match the original request"))
          } else {
            val asyncReply = originalMessages.collectFirst {
              case reply @ ToRequester(_) => reply
            }

            asyncReply.fold {
              // FIXME no async reply yet. Resend child requests w/o async reply
              wrapped(inbound).toRight(messageNotApplicable(inbound))
            } { reply =>
              Right(Seq(reply))
            }
          }
        }
      case inbound =>
        wrapped(inbound).toRight(messageNotApplicable(inbound))
    }

    private def sameMessage(a: NsiProviderMessage[NsiProviderOperation], b: NsiProviderMessage[NsiProviderOperation]): Boolean = {
      // JAXB documents cannot be compared directly due to broken equals implementation of the DOM tree.
      // Serialize both messages to XML and compare the resulting strings instead.
      import NsiSoapConversions._
      val conversion = NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(DocumentToString)
      conversion(a) == conversion(b)
    }

    private def PersistMessages(wrapped: InboundMessage => Option[Seq[OutboundMessage]]): InboundMessage => Option[Seq[OutboundMessage]] = { inbound =>
      val result = wrapped(inbound)
      result.foreach { outbound =>
        messageStore.storeAll(connection.id, inbound +: outbound)
      }
      result
    }

    private def updateChildConnection(message: InboundMessage): Unit = message match {
      case FromProvider(NsiRequesterMessage(_, ReserveConfirmed(connectionId, _))) => addChildConnectionId(self, connectionId)
      case FromProvider(NsiRequesterMessage(_, ReserveFailed(body))) => addChildConnectionId(self, body.getConnectionId)
      case _ =>
    }

    private def messageNotApplicable(message: InboundMessage): Any = message match {
      case FromRequester(NsiProviderMessage(headers, message)) => ServiceException(NsiError.InvalidTransition.toServiceException(Configuration.Nsa))
      case FromProvider(NsiRequesterMessage(headers, message)) => ServiceException(NsiError.InvalidTransition.toServiceException(Configuration.Nsa))
      case FromPce(message)                                    => 400
      case AckFromProvider(_) | _: MessageDeliveryFailure      => 500
    }
  }
}
