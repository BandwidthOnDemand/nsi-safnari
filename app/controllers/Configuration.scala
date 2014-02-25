package controllers

import play.api.Play.current
import scala.concurrent.duration._

object Configuration {
  lazy val Nsa = current.configuration.getString("safnari.nsa").getOrElse(sys.error("safnari.nsa not set"))
  def BaseUrl = current.configuration.getString("nsi.base.url").getOrElse(sys.error("nsi.base.url option is not set"))

  val Use2WayTLS = current.configuration.getBoolean("nsi.twoway.tls").getOrElse(sys.error("Setting the nsi.twoway.tls option is mandatory"))

  lazy val AsyncReplyTimeout = readFiniteDuration("safnari.async.reply.timeout")
  def ConnectionExpirationTime = readFiniteDuration("safnari.connection.expiration.time")

  private def readFiniteDuration(key: String): FiniteDuration = current.configuration.getString(key).map(Duration.apply) match {
    case Some(fd: FiniteDuration) => fd
    case Some(_)                  => sys.error(s"$key is not finite")
    case None                     => sys.error(s"$key not set")
  }
}
