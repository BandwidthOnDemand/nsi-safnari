package controllers

import play.api.mvc._
import play.api.http.HeaderNames._
import play.api.http.ContentTypes
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter
import scala.util.control.NonFatal

object DiscoveryService extends Controller {

  private val startTime = DateTime.now
  private val contentType = "application/vnd.ogf.nsi.nsa.v2+xml"

  private val timeZoneCode = "GMT"
  private val parsableTimezoneCode = s" $timeZoneCode"
  private val rfc1123Formatter = DateTimeFormat.forPattern(s"EEE, dd MMM yyyy HH:mm:ss '$timeZoneCode'").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))
  private val rfc1123Parser = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss").withLocale(java.util.Locale.ENGLISH).withZone(DateTimeZone.forID(timeZoneCode))

  def index = Action { implicit request =>
    def parseDate(date: String): Option[DateTime] = try {
      //jodatime does not parse timezones, so we handle that manually
      val d = rfc1123Parser.parseDateTime(date.replace(parsableTimezoneCode, ""))
      Some(d)
    } catch {
      case NonFatal(_) => None
    }

    val sendDocument = request.headers.get(IF_MODIFIED_SINCE).flatMap(parseDate).map(modifiedSince => startTime.isAfter(modifiedSince)).getOrElse(true)

    if (sendDocument)
      Ok(discoveryDocument).withHeaders(LAST_MODIFIED -> rfc1123Formatter.print(startTime)).as(ContentTypes.withCharset(contentType))
    else
       NotModified
  }

  def discoveryDocument(implicit request: RequestHeader) = {
    val secure = request.headers.get(X_FORWARDED_PROTO) == Some("https")
    val providerUrl = routes.ConnectionProvider.request.absoluteURL(secure)

    <nsa:nsa
        xmlns:vcard="urn:ietf:params:xml:ns:vcard-4.0"
        xmlns:nsa="http://schemas.ogf.org/nsi/2014/02/discovery/nsa"
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
    </nsa:nsa>
  }

}