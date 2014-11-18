package controllers

import play.api.Play.current
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import org.joda.time.DateTime
import java.net.URI
import com.typesafe.config.ConfigUtil
import nl.surfnet.safnari._

object Configuration {
  val StartTime = DateTime.now
  def NsaId = getStringOrFail("safnari.nsa.id")
  def NsaName = getStringOrFail("safnari.nsa.name")
  def AdminContactGiven = getStringOrFail("safnari.adminContact.given")
  def AdminContactSurname = getStringOrFail("safnari.adminContact.surname")
  def AdminContact = s"$AdminContactGiven $AdminContactSurname"
  def AdminContactProdid = getStringOrFail("safnari.adminContact.prodid")
  def Use2WayTLS = current.configuration.getBoolean("nsi.twoway.tls").getOrElse(sys.error("nsi.twoway.tls option is not set"))
  def VersionString = s"${BuildInfo.version} (${BuildInfo.gitHeadCommitSha})"
  def Longitude = getStringOrFail("safnari.location.longitude")
  def Latitude = getStringOrFail("safnari.location.latitude")
  def AsyncReplyTimeout = readFiniteDuration("safnari.async.reply.timeout")
  def NetworkId = current.configuration.getString("safnari.network.id")
  def NetworkUrl = current.configuration.getString("safnari.network.url")
  def DdsUrl = current.configuration.getString("safnari.dds.url")
  def PceAlgorithm: PathComputationAlgorithm = current.configuration.getString("pce.algorithm").flatMap(PathComputationAlgorithm.parse).getOrElse(sys.error("pce.algorithm option is not set or invalid"))
  def PceEndpoint = getStringOrFail("pce.endpoint")

  def PeersWith: Seq[PeerEntity] = {
    val configOption = current.configuration.getConfigList("safnari.peersWith")
    configOption.toSeq.flatMap(_.asScala.map( peer => PeerEntity(peer.getString("id"), peer.getString("dn")) ))
  }

  // Web page footer information for main.scala.html.
  def WebParams: Map[String,String] = Map("favicon" -> getStringOrFail("web.favicon"), "footer" -> getStringOrFail("web.footer"), "contactURL" -> getStringOrFail("web.contactURL"), "contactText" -> getStringOrFail("web.contactText"))

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
