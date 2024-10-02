/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
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

import akka.actor.ActorRef
import akka.pattern.ask
import controllers.ActorSupport._
import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal._
import nl.surfnet.safnari.PathComputationAlgorithm
import nl.surfnet.safnari.ReachabilityTopologyEntry
import play.api.http.ContentTypes
import play.api.mvc._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

class DiscoveryService(pceRequester: ActorRef, configuration: Configuration)(implicit ec: ExecutionContext) extends InjectedController {
  private val ContentTypeDiscoveryDocument = "application/vnd.ogf.nsi.nsa.v1+xml"
  private val rfc1123Formatter = DateTimeFormatter.RFC_1123_DATE_TIME.withLocale(java.util.Locale.ENGLISH).withZone(ZoneId.of("GMT"))

  def index = Action.async { implicit request =>
    def parseDate(date: String): Option[Instant] = try {
      val d = ZonedDateTime.parse(date, rfc1123Formatter).toInstant
      Some(d)
    } catch {
      case NonFatal(_) => None
    }

    (pceRequester ? ReachabilityCheck).mapTo[Try[(Seq[ReachabilityTopologyEntry], Instant)]] map {
      case Success((reachability, lastModified)) =>
        val haveLatest = request.headers.get(IF_MODIFIED_SINCE).flatMap(parseDate).exists(ifModifiedSince => !ifModifiedSince.isBefore(lastModified.`with`(ChronoField.MILLI_OF_SECOND, 0)))

        if (haveLatest)
          NotModified
        else
          Ok(discoveryDocument(reachability, lastModified)).withHeaders(LAST_MODIFIED -> rfc1123Formatter.format(lastModified)).as(ContentTypes.withCharset(ContentTypeDiscoveryDocument))
      case Failure(e) =>
        ServiceUnavailable(e.getMessage())
    }
  }

  def discoveryDocument(reachabilityEntries: Seq[ReachabilityTopologyEntry], lastModified: Instant): xml.Elem = {
    val providerUrl = configuration.providerServiceUrl
    val requesterUrl = configuration.requesterServiceUrl

    <nsa:nsa
        xmlns:vcard="urn:ietf:params:xml:ns:vcard-4.0"
        xmlns:nsa="http://schemas.ogf.org/nsi/2014/02/discovery/nsa"
        xmlns:gns="http://nordu.net/namespaces/2013/12/gnsbod"
        id={ configuration.NsaId }
        version={ lastModified.toString() }>
      <name>{ configuration.NsaName }</name>
      <softwareVersion>{ configuration.VersionString }</softwareVersion>
      <startTime>{ configuration.StartTime.toString() }</startTime>
      <adminContact>
        <vcard:vcard>
          <vcard:uid>
            <vcard:uri>{ providerUrl }#adminContact</vcard:uri>
          </vcard:uid>
          <vcard:prodid>
            <vcard:text>{ configuration.AdminContactProdid } </vcard:text>
          </vcard:prodid>
          <vcard:rev>
            <vcard:timestamp>{ DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'").format(configuration.StartTime) }</vcard:timestamp>
          </vcard:rev>
          <vcard:kind>
            <vcard:text>individual</vcard:text>
          </vcard:kind>
          <vcard:fn>
            <vcard:text>{ configuration.AdminContact }</vcard:text>
          </vcard:fn>
          <vcard:n>
            <vcard:surname>{ configuration.AdminContactSurname }</vcard:surname>
            <vcard:given>{ configuration.AdminContactGiven }</vcard:given>
          </vcard:n>
        </vcard:vcard>
      </adminContact>
      <location>
        <longitude>{ configuration.Longitude }</longitude>
        <latitude>{ configuration.Latitude }</latitude>
      </location>
      { configuration.NetworkId match {
          case Some(id) => <networkId>{ id }</networkId>
          case _ =>
        }
      }
      { configuration.NetworkUrl match {
          case Some(url) =>
      <interface>
        <type>application/vnd.ogf.nsi.topology.v2+xml</type>
        <href>{ url }</href>
      </interface>
          case _ =>
        }
      }
      { configuration.DdsUrl match {
          case Some(url) =>
      <interface>
        <type>application/vnd.ogf.nsi.dds.v1+xml</type>
        <href>{ url }</href>
      </interface>
          case _ =>
        }
      }
      <interface>
        <type>application/vnd.ogf.nsi.cs.v2.requester+soap</type>
        <href>{ requesterUrl }</href>
      </interface>
      <interface>
        <type>application/vnd.ogf.nsi.cs.v2.provider+soap</type>
        <href>{ providerUrl }</href>
      </interface>
      { if (configuration.NetworkId.isDefined) {
          <feature type="vnd.ogf.nsi.cs.v2.role.uPA"/>
        }
      }
      <feature type="vnd.ogf.nsi.cs.v2.role.aggregator"/>
      { for (peer <- configuration.PeersWith) yield {
          peer.id match {
            case Some(id) => <peersWith>{id}</peersWith>
            case _ =>
          }
        }
      }
      { if (configuration.PceAlgorithm != PathComputationAlgorithm.Tree && configuration.NetworkId.isDefined && !reachabilityEntries.isEmpty) {
        <other>
          <gns:TopologyReachability>
            { reachabilityEntries.map(entry => <Topology id={ entry.id } cost={ entry.cost.toString } />) }
          </gns:TopologyReachability>
        </other>
      }
    }
    </nsa:nsa>
  }
}
