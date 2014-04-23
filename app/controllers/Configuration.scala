package controllers

import play.api.Play.current
import scala.concurrent.duration._
import java.net.URI
import com.typesafe.config.ConfigUtil
import nl.surfnet.safnari._

object Configuration {
  lazy val NsaId = getStringOrFail("safnari.nsa.id")
  lazy val NsaName = getStringOrFail("safnari.nsa.name")
  lazy val AdminContactGiven = getStringOrFail("safnari.adminContact.given")
  lazy val AdminContactSurname = getStringOrFail("safnari.adminContact.surname")
  lazy val AdminContact = s"$AdminContactGiven $AdminContactSurname"
  lazy val AdminContactProdid = getStringOrFail("safnari.adminContact.prodid")
  lazy val Use2WayTLS = current.configuration.getBoolean("nsi.twoway.tls").getOrElse(sys.error("nsi.twoway.tls option is not set"))
  lazy val VersionString = s"${BuildInfo.version} (${BuildInfo.gitHeadCommitSha})"
  lazy val Longitude = getStringOrFail("safnari.location.longitude")
  lazy val Latitude = getStringOrFail("safnari.location.latitude")
  lazy val AsyncReplyTimeout = readFiniteDuration("safnari.async.reply.timeout")
  lazy val NetworkId = current.configuration.getString("safnari.network.id")
  lazy val NetworkUrl = current.configuration.getString("safnari.network.url")
  lazy val PceAlgorithm: PathComputationAlgorithm = current.configuration.getString("pce.algorithm").flatMap(PathComputationAlgorithm.parse).getOrElse(sys.error("pce.algorithm option is not set or invalid"))
  lazy val PceEndpoint = getStringOrFail("pce.endpoint")

  def ConnectionExpirationTime = readFiniteDuration("safnari.connection.expiration.time")

  def BaseUrl = getStringOrFail("nsi.base.url")

  private def getStringOrFail(property: String) = current.configuration.getString(property).getOrElse(sys.error(s"$property is not set"))

  private def readFiniteDuration(key: String): FiniteDuration = current.configuration.getString(key).map(Duration.apply) match {
    case Some(fd: FiniteDuration) => fd
    case Some(_)                  => sys.error(s"$key is not finite")
    case None                     => sys.error(s"$key not set")
  }

  def translateToStunnelAddress(provider: ProviderEndPoint): URI =
    current.configuration.getString(ConfigUtil.joinPath("nsi", "tlsmap", provider.nsa)) match {
      case Some(hostAndPort) =>
        val splitted = hostAndPort.split(":")
        new URI("http", null, splitted(0), Integer.parseInt(splitted(1)), provider.url.getPath, provider.url.getQuery, provider.url.getFragment)
      case None =>
        throw new IllegalArgumentException(s"No stunnel detour configured for NSA ${provider.nsa} while TLS was enabled.")
    }
}