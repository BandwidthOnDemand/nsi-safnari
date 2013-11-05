package controllers

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import nl.surfnet.safnari._
import play.Logger
import scala.concurrent._
import scala.concurrent.duration.{ Duration, DurationInt }
import scala.concurrent.stm._

class ConnectionManager(connectionFactory: (ConnectionId, NsiProviderMessage[InitialReserve]) => (ActorRef, ConnectionEntity)) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]
  private val childConnections = TMap.empty[ConnectionId, ActorRef]
  private val messageStore = new MessageStore[Message]()

  def add(connectionId: ConnectionId, connection: ActorRef) = connections.single(connectionId) = connection

  def get(connectionId: ConnectionId): Option[ActorRef] = connections.single.get(connectionId)

  def find(connectionIds: Seq[ConnectionId]): Seq[ActorRef] = connections.single.filterKeys(connectionIds.contains).values.toSeq

  private def addChildConnectionId(connection: ActorRef, childConnectionId: ConnectionId) {
    childConnections.single(childConnectionId) = connection
  }

  def findByChildConnectionId(connectionId: ConnectionId): Option[ActorRef] = childConnections.single.get(connectionId)

  def all: Seq[ActorRef] = connections.single.values.toSeq

  def restore(implicit actorSystem: ActorSystem, executionContext: ExecutionContext) {
    Logger.info("Start replaying of connection messages")
    Await.ready(Future.sequence(for {
      (connectionId, messages @ (FromRequester(NsiProviderMessage(headers, initialReserve: InitialReserve)) +: _)) <- messageStore.loadEverything()
      connection = createConnection(connectionId, NsiProviderMessage(headers, initialReserve))
    } yield {
      connection ? Replay(messages)
    }), Duration.Inf)

    Logger.info("Replay completed")
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
    add(connectionId, storingActor)
    storingActor
  }

  private[controllers] case class Replay(messages: Seq[Message])

  private class ConnectionActor(connection: ConnectionEntity, output: ActorRef) extends Actor {
    override def receive = LoggingReceive {
      case 'query              => sender ! connection.query
      case 'querySegments      => sender ! connection.segments
      case 'queryNotifications => sender ! connection.notifications

      case inbound: InboundMessage =>
        val result = connection.process(inbound)

        val response = result match {
          case None =>
            messageNotApplicable(inbound)
          case Some(outbound) =>
            messageStore.storeAll(connection.id, inbound +: outbound)
            updateChildConnection(inbound)
            outbound.foreach(connection.process)

            outbound.foreach(output ! _)

            inbound match {
              case FromRequester(NsiProviderMessage(_, _: InitialReserve)) => ReserveResponse(connection.id)
              case FromRequester(_) | FromProvider(_) => GenericAck()
              case FromPce(_) => 200
              case _: AckFromProvider => ()
            }
        }

        sender ! response

      case Replay(messages) =>
        Logger.info(s"Replaying ${messages.size} messages for connection ${connection.id}")
        messages.foreach {
          case inbound: InboundMessage =>
            updateChildConnection(inbound)
            connection.process(inbound).getOrElse {
              Logger.warn(s"Connection ${connection.id} failed to replay message $inbound")
            }
          case outbound: OutboundMessage =>
            connection.process(outbound)
        }
        sender ! (())
    }

    private def updateChildConnection(message: InboundMessage): Unit = message match {
      case FromProvider(NsiRequesterMessage(_, ReserveConfirmed(connectionId, _))) => addChildConnectionId(self, connectionId)
      case FromProvider(NsiRequesterMessage(_, ReserveFailed(body)))               => addChildConnectionId(self, body.getConnectionId)
      case _ =>
    }

    private def messageNotApplicable(message: InboundMessage): Any = message match {
      case FromRequester(NsiProviderMessage(headers, message)) => ServiceException(NsiError.InvalidTransition.toServiceException(Configuration.Nsa))
      case FromProvider(NsiRequesterMessage(headers, message)) => ServiceException(NsiError.InvalidTransition.toServiceException(Configuration.Nsa))
      case FromPce(message)                                    => 400
      case _: AckFromProvider                                  => 500
    }
  }
}
