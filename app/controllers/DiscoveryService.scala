package controllers

import akka.actor.ActorRef
import akka.pattern.ask
import controllers.ActorSupport._
import nl.surfnet.safnari.{PathComputationAlgorithm, ReachabilityTopologyEntry}
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import play.api.http.ContentTypes
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

class DiscoveryService(pceRequester: ActorRef) extends Controller {
  private val ContentTypeDiscoveryDocument = "application/vnd.ogf.nsi.nsa.v2+xml"
  private val timeZoneCode = "GMT"
  private val parseableTimezoneCode = s" $timeZoneCode"
  private val rfc1123Formatter = DateTimeFormat.forPattern(s"EEE, dd MMM yyyy HH:mm:ss '$timeZoneCode'").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))
  private val rfc1123Parser = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))

  def index = Action.async { implicit request =>
    def parseDate(date: String): Option[DateTime] = try {
      //jodatime does not parse timezones, so we handle that manually
      val d = rfc1123Parser.parseDateTime(date.replace(parseableTimezoneCode, ""))
      Some(d)
    } catch {
      case NonFatal(_) => None
    }

    (pceRequester ? 'reachability).mapTo[Try[(Seq[ReachabilityTopologyEntry], DateTime)]] map {
      case Success((reachability, lastModified)) =>
        val haveLatest = request.headers.get(IF_MODIFIED_SINCE).flatMap(parseDate).exists(ifModifiedSince => !ifModifiedSince.isBefore(lastModified.withMillisOfSecond(0)))

        if (haveLatest)
          NotModified
        else
          Ok(discoveryDocument(reachability)).withHeaders(LAST_MODIFIED -> rfc1123Formatter.print(lastModified)).as(ContentTypes.withCharset(ContentTypeDiscoveryDocument))
      case Failure(e) =>
        ServiceUnavailable(e.getMessage())
    }
  }

  def discoveryDocument(reachabilityEntries: Seq[ReachabilityTopologyEntry])(implicit request: RequestHeader): xml.Elem = {
    val secure = request.headers.get(X_FORWARDED_PROTO) == Some("https")
    val providerUrl = routes.ConnectionProvider.request.absoluteURL(secure)
    val requesterUrl = routes.ConnectionRequester.request.absoluteURL(secure)

    <nsa:nsa
        xmlns:vcard="urn:ietf:params:xml:ns:vcard-4.0"
        xmlns:nsa="http://schemas.ogf.org/nsi/2014/02/discovery/nsa"
        xmlns:gns="http://nordu.net/namespaces/2013/12/gnsbod"
        id={ Configuration.NsaId }
        version={ Configuration.StartTime.toString() }>
      <name>{ Configuration.NsaName }</name>
      <softwareVersion>{ Configuration.VersionString }</softwareVersion>
      <startTime>{ Configuration.StartTime.toString() }</startTime>
      <adminContact>
        <vcard:vcard>
          <vcard:uid>
            <vcard:uri>{ providerUrl }#adminContact</vcard:uri>
          </vcard:uid>
          <vcard:prodid>
            <vcard:text>{ Configuration.AdminContactProdid } </vcard:text>
          </vcard:prodid>
          <vcard:rev>
            <vcard:timestamp>{ DateTimeFormat.forPattern("yyyyMMdd'T'HHmmss'Z'").print(Configuration.StartTime) }</vcard:timestamp>
          </vcard:rev>
          <vcard:kind>
            <vcard:text>individual</vcard:text>
          </vcard:kind>
          <vcard:fn>
            <vcard:text>{ Configuration.AdminContact }</vcard:text>
          </vcard:fn>
          <vcard:n>
            <vcard:surname>{ Configuration.AdminContactSurname }</vcard:surname>
            <vcard:given>{ Configuration.AdminContactGiven }</vcard:given>
          </vcard:n>
        </vcard:vcard>
      </adminContact>
      <location>
        <longitude>{ Configuration.Longitude }</longitude>
        <latitude>{ Configuration.Latitude }</latitude>
      </location>
      { Configuration.NetworkId match {
          case Some(id) => <networkId>{ id }</networkId>
          case _ =>
        }
      }
      { Configuration.NetworkUrl match {
          case Some(url) =>
      <interface>
        <type>application/vnd.ogf.nsi.topology.v2+xml</type>
        <href>{ url }</href>
      </interface>
          case _ =>
        }
      }
      { Configuration.DdsUrl match {
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
      { if (Configuration.NetworkId.isDefined) {
          <feature type="vnd.ogf.nsi.cs.v2.role.uPA"/>
        }
      }
      <feature type="vnd.ogf.nsi.cs.v2.role.aggregator"/>
      { for (peer <- Configuration.PeersWith) yield {
          peer.id match {
            case Some(id) => <peersWith>{id}</peersWith>
            case _ =>
          }
        }
      }
      { if (Configuration.PceAlgorithm == nl.surfnet.safnari.ChainAlgorithm && Configuration.NetworkId.isDefined && !reachabilityEntries.isEmpty) {
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
