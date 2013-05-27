package controllers

import nl.surfnet.safnari._
import scala.concurrent.stm.TMap
import akka.actor.ActorRef
import akka.pattern.ask
import nl.surfnet.safnari.SegmentKnown
import akka.util.Timeout
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Await
import scala.concurrent.Future
import akka.actor.Props
import java.net.URI
import akka.actor.Actor
import play.api.libs.concurrent.Akka
import play.api.Play.current

class ConnectionManager(connectionFactory: (ConnectionId, Reserve) => ((ActorRef, Message, ActorRef) => Unit) => ActorRef) {
  implicit val timeout = Timeout(2.seconds)

  private val connections = TMap.empty[ConnectionId, ActorRef]

  def add(connectionId: ConnectionId, connection: ActorRef) = connections.single(connectionId) = connection

  def get(connectionId: ConnectionId): Option[ActorRef] = connections.single.get(connectionId)

  def find(connectionIds: Seq[String]): Seq[ActorRef] = connections.single.filterKeys(connectionIds.contains).values.toSeq

  def findBySegment(connectionId: ConnectionId): Future[Option[ActorRef]] = {
    val knowsAboutSegment = connections.single.values.map(c => c ? SegmentKnown(connectionId) map (c -> _.asInstanceOf[Boolean]))
    Future.find(knowsAboutSegment)(_._2) map (_.map(_._1))
  }

  def all: Seq[ActorRef] = connections.single.values.toSeq

  def restore() {
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

  def findOrCreateConnection(request: NsiProviderOperation): (ConnectionId, Option[ActorRef]) = (request, request.optionalConnectionId) match {
    case (_, Some(connectionId)) =>
      (connectionId, get(connectionId))
    case (initialReserve: Reserve, None) =>
      val connectionId = newConnectionId
      val connection = createConnection(connectionId, initialReserve)
      (connectionId, Some(connection))
    case _ =>
      sys.error("illegal initial message")
  }

  private def createConnection(connectionId: ConnectionId, initialReserve: Reserve): ActorRef = {
    val correlationIdGenerator = Uuid.deterministicUuidGenerator(connectionId.##)
    val connectionActor = connectionFactory(connectionId, initialReserve) { (sender, message, outbound) =>
      if (replaying) message.pp("Dropped in replay mode")
      else outbound.!(message)(sender)
    }
    val storingActor = Akka.system.actorOf(Props(new Actor {
      override def receive = {
        case message =>
          val store = message match {
            case message: FromRequester => Some(message)
            case message: FromProvider  => Some(message)
            case message: FromPce       => Some(message)
            case message =>
              message.pp("Not persisted")
              None
          }
          store foreach { messageStore.store(connectionId, _) }
          connectionActor forward message
      }
    }))
    add(connectionId, storingActor)
    storingActor
  }

}
