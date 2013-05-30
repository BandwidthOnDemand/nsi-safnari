package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import nl.surfnet.safnari._
import scala.concurrent._
import scala.concurrent.duration.{ Duration, DurationInt }
import scala.concurrent.stm._
import play.Logger

class ConnectionManager(connectionFactory: (ConnectionId, Reserve) => (ActorRef, ConnectionEntity)) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]
  private val messageStore = new MessageStore[Message]()

  def add(connectionId: ConnectionId, connection: ActorRef) = connections.single(connectionId) = connection

  def get(connectionId: ConnectionId): Option[ActorRef] = connections.single.get(connectionId)

  def find(connectionIds: Seq[String]): Seq[ActorRef] = connections.single.filterKeys(connectionIds.contains).values.toSeq

  def findBySegment(connectionId: ConnectionId)(implicit executionContext: ExecutionContext): Future[Option[ActorRef]] = {
    val knowsAboutSegment = connections.single.values.map(c => c ? SegmentKnown(connectionId) map (c -> _.asInstanceOf[Boolean]))
    Future.find(knowsAboutSegment)(_._2) map (_.map(_._1))
  }

  def all: Seq[ActorRef] = connections.single.values.toSeq

  def restore(implicit actorSystem: ActorSystem, executionContext: ExecutionContext) {
    Logger.info("Start replaying of connection messages")
    Await.ready(Future.sequence(for {
      (connectionId, messages @ (FromRequester(initialReserve: Reserve) +: _)) <- messageStore.loadEverything()
      connection = createConnection(connectionId, initialReserve)
    } yield {
      connection ? Replay(messages)
    }), Duration.Inf)

    Logger.info("Replay completed")
  }

  def findOrCreateConnection(request: NsiProviderOperation)(implicit actorSystem: ActorSystem): (ConnectionId, Option[ActorRef]) = (request, request.optionalConnectionId) match {
    case (_, Some(connectionId)) =>
      (connectionId, get(connectionId))
    case (initialReserve: Reserve, None) =>
      val connectionId = newConnectionId
      val connection = createConnection(connectionId, initialReserve)
      (connectionId, Some(connection))
    case _ =>
      sys.error("illegal initial message")
  }

  private def createConnection(connectionId: ConnectionId, initialReserve: Reserve)(implicit actorSystem: ActorSystem): ActorRef = {
    val (output, connection) = connectionFactory(connectionId, initialReserve)

    val storingActor = actorSystem.actorOf(Props(new ConnectionActor(connection, output)))
    add(connectionId, storingActor)
    storingActor
  }

  private case class SegmentKnown(segmentId: ConnectionId)

  private case class Replay(messages: Seq[Message])

  private class ConnectionActor(connection: ConnectionEntity, output: ActorRef) extends Actor {
    override def receive = {
      case 'query                     => sender ! connection.query
      case 'querySegments             => sender ! connection.segments

      case SegmentKnown(connectionId) => sender ! connection.rsm.segmentKnown(connectionId)

      case inbound: InboundMessage =>
        val result = connection.process(inbound)

        val response = result match {
          case None =>
            messageNotApplicable(inbound)
          case Some(outbound) =>
            messageStore.storeAll(connection.id, inbound +: outbound)

            outbound.foreach(output ! _)

            inbound match {
              case FromRequester(reserve: Reserve) => ReserveResponse(reserve.headers.asReply, connection.id)
              case FromRequester(request)          => request.ack
              case FromProvider(request)           => request.ack
              case FromPce(request)                => 200
            }
        }

        sender ! response

      case Replay(messages) =>
        Logger.info(s"Replaying ${messages.size} messages for connection ${connection.id}")
        messages.foreach {
          case message: InboundMessage =>
            connection.process(message).getOrElse {
              Logger.warn(s"Connection ${connection.id} failed to replay message $message")
            }
          case message: OutboundMessage =>
          // Ignore.
        }
    }

    private def messageNotApplicable(message: InboundMessage) = message match {
      case FromRequester(message) => ServiceException(message.headers.asReply, NsiError.InvalidState.toServiceException("NSA-ID"))
      case FromProvider(message)  => ServiceException(message.headers.asReply, NsiError.InvalidState.toServiceException("NSA-ID"))
      case FromPce(message)       => 400
    }
  }
}
