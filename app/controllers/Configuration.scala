/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
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
