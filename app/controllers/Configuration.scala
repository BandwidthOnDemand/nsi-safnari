package controllers

import play.api.Play.current
import scala.concurrent.duration._

object Configuration {
  lazy val Nsa = current.configuration.getString("safnari.requester.nsa").getOrElse(sys.error("safnari.requester.nsa not set"))
  lazy val AsyncReplyTimeout = current.configuration.getString("safnari.async.reply.timeout").map(Duration.apply) match {
    case Some(fd: FiniteDuration) => fd
    case Some(_)                  => sys.error("safnari.async.reply.timeout is not finite")
    case None                     => sys.error("safnari.async.reply.timeout not set")
  }
  lazy val BaseUrl = current.configuration.getString("nsi.base.url").getOrElse(sys.error("nsi.base.url option is not set"))
}
