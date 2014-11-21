package controllers

import akka.util.Timeout
import play.api.libs.concurrent.Akka
import scala.concurrent.duration._

private[controllers] object ActorSupport {
  implicit val timeout = Timeout(30.seconds)
  implicit def actorSystem(implicit app: play.api.Application) = Akka.system
}
