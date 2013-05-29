package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import nl.surfnet.safnari._
import scala.concurrent._
import scala.concurrent.duration.{ Duration, DurationInt }
import scala.concurrent.stm._

class ConnectionManager(connectionFactory: (ConnectionId, Reserve) => (ActorRef, ConnectionEntity)) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]

  def add(connectionId: ConnectionId, connection: ActorRef) = connections.single(connectionId) = connection

  def get(connectionId: ConnectionId): Option[ActorRef] = connections.single.get(connectionId)

  def find(connectionIds: Seq[String]): Seq[ActorRef] = connections.single.filterKeys(connectionIds.contains).values.toSeq

  def findBySegment(connectionId: ConnectionId)(implicit executionContext: ExecutionContext): Future[Option[ActorRef]] = {
    val knowsAboutSegment = connections.single.values.map(c => c ? SegmentKnown(connectionId) map (c -> _.asInstanceOf[Boolean]))
    Future.find(knowsAboutSegment)(_._2) map (_.map(_._1))
  }

  def all: Seq[ActorRef] = connections.single.values.toSeq

  def restore(implicit actorSystem: ActorSystem, executionContext: ExecutionContext) {
    replaying = true
    Await.ready(Future.sequence(for {
      (connectionId, messages @ (FromRequester(initialReserve: Reserve) +: _)) <- messageStore.loadEverything()
      connection = createConnection(connectionId, initialReserve)
      message <- messages
    } yield {
      message.getClass.getName.pp("Replaying message")
      connection ? message
    }), Duration.Inf)

    replaying = false
    "Replay completed".pp
  }

  private val messageStore = new MessageStore[Message]()
  @volatile private var replaying = false

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
    val (outbound, connection) = connectionFactory(connectionId, initialReserve)

    val storingActor = actorSystem.actorOf(Props(new Actor {
      override def receive = {
        case 'query                     => sender ! connection.query
        case 'querySegments             => sender ! connection.segments

        case SegmentKnown(connectionId) => sender ! connection.rsm.segmentKnown(connectionId)

        case message: InboundMessage =>
          if (!replaying) {
            val store = message match {
              case message: FromRequester => Some(message)
              case message: FromProvider  => Some(message)
              case message: FromPce       => Some(message)
              case message =>
                message.pp("Not persisted")
                None
            }
            store foreach { messageStore.store(connectionId, _) }
          }

          val output = connection.process(message)

          output match {
            case None =>
              sender ! messageNotApplicable(message)
            case Some(messages) =>
              if (!replaying) messages.foreach(outbound ! _)
              message match {
                case FromRequester(reserve: Reserve) => sender ! ReserveResponse(reserve.headers.asReply, connectionId)
                case FromRequester(request)          => sender ! request.ack
                case FromProvider(request)           => sender ! request.ack
                case FromPce(request)                => sender ! 200
              }
          }
      }

      private def messageNotApplicable(message: InboundMessage) = message match {
        case FromRequester(message) => ServiceException(message.headers.asReply, NsiError.InvalidState.toServiceException("NSA-ID"))
        case FromProvider(message)  => ServiceException(message.headers.asReply, NsiError.InvalidState.toServiceException("NSA-ID"))
        case FromPce(message)       => 400
      }
    }))
    add(connectionId, storingActor)
    storingActor
  }

}
