package controllers

import play.api.mvc._
import play.api.http.ContentTypes
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat
import scala.util.control.NonFatal
import play.api.Play._
import scala.Some
import play.api.libs.ws.WS
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

object DiscoveryService extends Controller {

  case class ReachabilityTopologyEntry(id: String, cost: Int)

  private val startTime = DateTime.now
  private val contentType = "application/xml"

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
    val reachabilityEndpoint = current.configuration.getString("pce.endpoint").getOrElse(sys.error("pce.endpoint configuration property is not set")) + "/reachability"
    WS.url(reachabilityEndpoint).get().map { response =>
      implicit val reachabilityTopologyEntryReader = Json.reads[ReachabilityTopologyEntry]
      val jsValue = response.json
      val reachabilityEntries = (jsValue \ "reachability").as[List[ReachabilityTopologyEntry]]
      val sendDocument = request.headers.get(IF_MODIFIED_SINCE).flatMap(parseDate).map(modifiedSince => startTime.isAfter(modifiedSince)).getOrElse(true)
      if (sendDocument)
        Ok(discoveryDocument(reachabilityEntries)).withHeaders(LAST_MODIFIED -> rfc1123Formatter.print(startTime)).as(ContentTypes.withCharset(contentType))
      else
        NotModified
    }
  }

  def discoveryDocument(reachabilityEntries: List[ReachabilityTopologyEntry])(implicit request: RequestHeader) = {
    val secure = request.headers.get(X_FORWARDED_PROTO) == Some("https")
    val providerUrl = routes.ConnectionProvider.request.absoluteURL(secure)

    <nsa:nsa
        xmlns:vcard="urn:ietf:params:xml:ns:vcard-4.0"
        xmlns:nsa="http://schemas.ogf.org/nsi/2014/02/discovery/nsa"
        xmlns:gns="http://nordu.net/namespaces/2013/12/gnsbod"
        id={ Configuration.NsaId }
        version={ startTime.toString() }>
      <name>{ Configuration.NsaName }</name>
      <softwareVersion>{ Configuration.VersionString }</softwareVersion>
      <startTime>{ startTime.toString() }</startTime>
      <adminContact>
        <vcard:vcard>
          <vcard:uid>
            <vcard:uri>{ providerUrl }#adminContact</vcard:uri>
          </vcard:uid>
          <vcard:prodid>
            <vcard:text>{ Configuration.AdminContactProdid } </vcard:text>
          </vcard:prodid>
          <vcard:rev>
            <vcard:timestamp>{ DateTimeFormat.forPattern("yyyyMMdd'T'HHmmss'Z'").print(DateTime.now) }</vcard:timestamp>
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
      <interface>
        <type>application/vnd.org.ogf.nsi.cs.v2+soap</type>
        <href> { providerUrl }</href>
      </interface>
      { if (Configuration.NetworkId.isDefined) {
          <feature type="vnd.ogf.nsi.cs.v2.role.uPA"/>
        }
      }
      <feature type="vnd.ogf.nsi.cs.v2.role.aggregator"/>
      <other>
        { reachabilityEntries match {
            case Nil =>
            case _ =>
              <gns:TopologyReachability>
                { reachabilityEntries.map { entry =>
                    <Topology id={ entry.id } cost={ entry.cost.toString }/>
                  }
                }
              </gns:TopologyReachability>
          }
        }
      </other>
    </nsa:nsa>
  }

}