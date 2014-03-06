package controllers

import play.api.Play.current
import scala.concurrent.duration._
import java.net.URI
import com.typesafe.config.ConfigUtil
import nl.surfnet.safnari.ProviderEndPoint

object Configuration {
  lazy val Nsa = current.configuration.getString("safnari.nsa").getOrElse(sys.error("safnari.nsa not set"))
  def BaseUrl = current.configuration.getString("nsi.base.url").getOrElse(sys.error("nsi.base.url option is not set"))
  val Use2WayTLS = current.configuration.getBoolean("nsi.twoway.tls").getOrElse(sys.error("nsi.twoway.tls option is not set"))

  lazy val AsyncReplyTimeout = readFiniteDuration("safnari.async.reply.timeout")
  def ConnectionExpirationTime = readFiniteDuration("safnari.connection.expiration.time")

  private def readFiniteDuration(key: String): FiniteDuration = current.configuration.getString(key).map(Duration.apply) match {
    case Some(fd: FiniteDuration) => fd
    case Some(_)                  => sys.error(s"$key is not finite")
    case None                     => sys.error(s"$key not set")
  }

  def translateToStunnelAddress(provider: ProviderEndPoint): URI =
    current.configuration.getString(ConfigUtil.joinPath("nsi", "tlsmap", provider.nsa)) match {
      case Some(hostAndPort) =>
        val splitted = hostAndPort.split(":")
        new URI("http", "", splitted(0), Integer.parseInt(splitted(1)), provider.url.getPath, provider.url.getQuery, provider.url.getFragment)
      case None =>
        throw new IllegalArgumentException(s"No stunnel detour configured for NSA ${provider.nsa} while TLS was enabled.")
    }

}