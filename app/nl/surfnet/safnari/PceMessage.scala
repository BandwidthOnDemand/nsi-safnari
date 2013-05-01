package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._04.connection.types._
import java.net.URI
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.UUID
import play.api.data.validation.ValidationError
import scala.util.Try
import javax.xml.datatype.XMLGregorianCalendar
import javax.xml.datatype.DatatypeFactory

sealed trait PceMessage {
  def correlationId: CorrelationId
}
case class PathComputationRequest(correlationId: CorrelationId, replyTo: URI, criteria: ReservationConfirmCriteriaType) extends PceMessage

sealed trait PceResponse extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId) extends PceResponse
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ComputedSegment]) extends PceResponse

sealed trait ProviderAuthentication
case object NoAuthentication extends ProviderAuthentication
case class BasicAuthentication(username: String, password: String) extends ProviderAuthentication
case class OAuthAuthentication(token: String) extends ProviderAuthentication

case class ComputedSegment(sourceStp: StpType, destinationStp: StpType, providerNsa: String, providerUrl: URI, authentication: ProviderAuthentication)

object PceMessage {
  implicit val CorrelationIdReads: Reads[CorrelationId] = Reads[CorrelationId] {
    case JsString(s) => CorrelationId.fromString(s).map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.correlation.id", s)) }
    case json        => JsError(ValidationError("bad.correlation.id", json))
  }
  implicit val CorrelationIdWrites: Writes[CorrelationId] = Writes { x => JsString(x.toString) }

  implicit val UriReads: Reads[URI] = Reads[URI] {
    case JsString(s) => Try { new URI(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.uri", s)) }
    case json        => JsError(ValidationError("bad.uri", json))
  }
  implicit val UriWriteS: Writes[URI] = Writes[URI] { x => JsString(x.toASCIIString()) }

  // FIXME labels
  implicit val StpTypeFormat: Format[StpType] = (
    (__ \ "network-id").format[String] and
    (__ \ "local-id").format[String] and
    (__ \ "labels").format[Option[JsObject]])(
      (networkId, localId, labels) => new StpType().withNetworkId(networkId).withLocalId(localId),
      stpType => (stpType.getNetworkId(), stpType.getLocalId(), None))

  implicit val ProviderAuthenticationFormat: Format[ProviderAuthentication] = (
    (__ \ "method").format[String] and
    (__ \ "username").format[Option[String]] and
    (__ \ "password").format[Option[String]] and
    (__ \ "token").format[Option[String]])(
      (method, username, password, token) => method match {
        case "none"   => NoAuthentication
        case "basic"  => BasicAuthentication(username.get, password.get)
        case "oauth2" => OAuthAuthentication(token.get)
        case _        => ???
      },
      {
        case NoAuthentication                        => ("none", None, None, None)
        case BasicAuthentication(username, password) => ("basic", Some(username), Some(password), None)
        case OAuthAuthentication(token)              => ("oauth2", None, None, Some(token))
      })

  implicit val ComputedSegmentFormat: Format[ComputedSegment] = (
    (__ \ "source-stp").format[StpType] and
    (__ \ "destination-stp").format[StpType] and
    (__ \ "nsa").format[String] and
    (__ \ "provider-url").format[URI] and
    (__ \ "auth").format[ProviderAuthentication])(ComputedSegment.apply, unlift(ComputedSegment.unapply))

  implicit val PceResponseFormat: Format[PceResponse] = (
    (__ \ "correlation-id").format[CorrelationId] and
    (__ \ "status").format[String] and
    (__ \ "path").format[Option[Seq[ComputedSegment]]])((correlationId, status, path) => status match {
      case "success" => PathComputationConfirmed(correlationId, path.getOrElse(Nil))
      case "failed"  => PathComputationFailed(correlationId)
    }, {
      case PathComputationConfirmed(correlationId, segments) =>
        (correlationId, "success", Some(segments))
      case PathComputationFailed(correlationId) =>
        (correlationId, "failed", None)
    })

  implicit val XMLGregorianCalendarReads: Reads[XMLGregorianCalendar] = Reads {
    case JsString(s) => Try { DatatypeFactory.newInstance().newXMLGregorianCalendar(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.timestamp", s)) }
    case json        => JsError(ValidationError("bad.timestamp", json))
  }
  implicit val XMLGregorianCalendarWrites: Writes[XMLGregorianCalendar] = Writes { x => JsString(x.toString) }

  implicit val PceRequestFormat: Format[PathComputationRequest] = (
    (__ \ "source-stp").format[StpType] and
    (__ \ "destination-stp").format[StpType] and
    (__ \ "start-time").format[Option[XMLGregorianCalendar]] and
    (__ \ "end-time").format[Option[XMLGregorianCalendar]] and
    (__ \ "bandwidth").format[Int] and
    (__ \ "reply-to").format[URI] and
    (__ \ "correlation-id").format[CorrelationId] and
    (__ \ "algorithm").format[Option[String]] and
    (__ \ "constraints").format[Seq[String]])((source, destination, start, end, bandwidth, replyTo, correlationId, algorithm, constraints) => {
      val criteria = new ReservationConfirmCriteriaType().
        withBandwidth(bandwidth).
        withPath(new PathType().withSourceSTP(source).withDestSTP(destination)).
        withSchedule(new ScheduleType().withStartTime(start.orNull).withEndTime(end.orNull))
      PathComputationRequest(correlationId, replyTo, criteria)
    }, {
      case PathComputationRequest(correlationId, replyTo, criteria) =>
        (criteria.getPath().getSourceSTP(),
          criteria.getPath().getDestSTP(),
          Option(criteria.getSchedule().getStartTime()),
          Option(criteria.getSchedule().getEndTime()),
          criteria.getBandwidth(),
          replyTo,
          correlationId,
          None,
          Nil)
    })
}
