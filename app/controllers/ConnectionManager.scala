package controllers

import nl.surfnet.safnari.ConnectionId
import scala.concurrent.stm.TMap
import akka.actor.ActorRef
import akka.pattern.ask
import nl.surfnet.safnari.SegmentKnown
import akka.util.Timeout
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Await
import scala.concurrent.Future

object ConnectionManager {
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

}
