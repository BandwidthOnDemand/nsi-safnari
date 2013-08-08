package nl.surfnet.safnari

import org.ogf.schemas.nsi._2013._07.connection.types._
import org.ogf.schemas.nsi._2013._07.services.types.StpType
import java.net.URI
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.UUID
import play.api.data.validation.ValidationError
import scala.util.Try
import javax.xml.datatype.XMLGregorianCalendar
import javax.xml.datatype.DatatypeFactory
import org.ogf.schemas.nsi._2013._07.services.point2point.P2PServiceBaseType

sealed trait PceMessage {
  def correlationId: CorrelationId
}
sealed trait PceRequest extends PceMessage
case class PathComputationRequest(correlationId: CorrelationId, replyTo: URI, schedule: ScheduleType, service: P2PServiceBaseType) extends PceRequest

sealed trait PceResponse extends PceMessage
case class PathComputationFailed(correlationId: CorrelationId, message: String) extends PceResponse
case class PathComputationConfirmed(correlationId: CorrelationId, segments: Seq[ComputedSegment]) extends PceResponse

sealed trait ProviderAuthentication
case object NoAuthentication extends ProviderAuthentication
case class BasicAuthentication(username: String, password: String) extends ProviderAuthentication
case class OAuthAuthentication(token: String) extends ProviderAuthentication

case class ProviderEndPoint(nsa: String, url: URI, authentication: ProviderAuthentication)

case class ComputedSegment(sourceStp: StpType, destinationStp: StpType, provider: ProviderEndPoint)

object PceMessage {
  private implicit class JsResultOps[A](js: JsResult[A]) {
    def clearPath = js.fold(JsError(_), JsSuccess(_, path = JsPath()))
  }

  implicit val CorrelationIdReads: Reads[CorrelationId] = Reads[CorrelationId] {
    case JsString(s) => CorrelationId.fromString(s).map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.correlation.id", s)) }
    case json        => JsError(ValidationError("bad.correlation.id", json))
  }
  implicit val CorrelationIdWrites: Writes[CorrelationId] = Writes { x => JsString(x.toString) }

  implicit val UriReads: Reads[URI] = Reads[URI] {
    case JsString(s) => Try { new URI(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.uri", s)) }
    case json        => JsError(ValidationError("bad.uri", json))
  }
  implicit val UriWrites: Writes[URI] = Writes[URI] { x => JsString(x.toASCIIString()) }

  // FIXME labels
  implicit val StpTypeFormat: Format[StpType] = (
    (__ \ "network-id").format[String] and
    (__ \ "local-id").format[String])(
      (networkId, localId) => new StpType().withNetworkId(networkId).withLocalId(localId),
      stpType => (stpType.getNetworkId(), stpType.getLocalId()))

  implicit val ProviderAuthenticationReads: Reads[ProviderAuthentication] = Reads { json =>
    (__ \ "method").read[String].reads(json) match {
      case JsSuccess("NONE", _)    => JsSuccess(NoAuthentication)
      case JsSuccess("BASIC", _)   => Json.reads[BasicAuthentication].reads(json).clearPath
      case JsSuccess("OAUTH2", _)  => Json.reads[OAuthAuthentication].reads(json).clearPath
      case JsSuccess(method, path) => JsError(path -> ValidationError("bad.authentication.method", method))
      case errors: JsError         => errors
    }
  }
  implicit val ProviderAuthenticationWrites: Writes[ProviderAuthentication] = Writes {
    case NoAuthentication                        => Json.obj("method" -> "NONE")
    case BasicAuthentication(username, password) => Json.obj("method" -> "BASIC", "username" -> username, "password" -> password)
    case OAuthAuthentication(token)              => Json.obj("method" -> "OAUTH2", "token" -> token)
  }

  implicit val ProviderEndPointFormat: OFormat[ProviderEndPoint] = (
    (__ \ "nsa").format[String] and
    (__ \ "provider-url").format[URI] and
    (__ \ "auth").format[ProviderAuthentication])(ProviderEndPoint.apply, unlift(ProviderEndPoint.unapply))

  implicit val ComputedSegmentFormat: Format[ComputedSegment] = (
    (__ \ "source-stp").format[StpType] and
    (__ \ "destination-stp").format[StpType] and
    ProviderEndPointFormat)(ComputedSegment.apply, unlift(ComputedSegment.unapply))

  implicit val PceResponseReads: Reads[PceResponse] = Reads { json =>
    (__ \ "status").read[String].reads(json) match {
      case JsSuccess("SUCCESS", _) => Json.fromJson[PathComputationConfirmed](json)
      case JsSuccess("FAILED", _)  => Json.fromJson[PathComputationFailed](json)
      case JsSuccess(status, path) => JsError(path -> ValidationError("bad.response.status", status))
      case errors: JsError         => errors
    }
  }
  implicit val PceResponseWrites: Writes[PceResponse] = Writes {
    case PathComputationConfirmed(correlationId, segments) => Json.obj("correlation-id" -> correlationId, "status" -> "SUCCESS", "path" -> segments)
    case PathComputationFailed(correlationId, message)     => Json.obj("correlation-id" -> correlationId, "status" -> "FAILED", "message" -> message)
  }

  implicit val PathComputationConfirmedReads: Reads[PathComputationConfirmed] = (
    (__ \ "correlation-id").read[CorrelationId] and
    (__ \ "path").read[Seq[ComputedSegment]]
  )(PathComputationConfirmed.apply _)

  implicit val PathComputationFailedReads: Reads[PathComputationFailed] = (
    (__ \ "correlation-id").read[CorrelationId] and
    (__ \ "message").read[String]
  )(PathComputationFailed.apply _)

  implicit val XMLGregorianCalendarReads: Reads[XMLGregorianCalendar] = Reads {
    case JsString(s) => Try { DatatypeFactory.newInstance().newXMLGregorianCalendar(s) }.map { x => JsSuccess(x) }.getOrElse { JsError(ValidationError("bad.timestamp", s)) }
    case json        => JsError(ValidationError("bad.timestamp", json))
  }
  implicit val XMLGregorianCalendarWrites: Writes[XMLGregorianCalendar] = Writes { x => JsString(x.toString) }

  implicit val PceRequestFormat: Format[PceRequest] = (
    (__ \ "source-stp").format[StpType] and
    (__ \ "destination-stp").format[StpType] and
    (__ \ "start-time").format[Option[XMLGregorianCalendar]] and
    (__ \ "end-time").format[Option[XMLGregorianCalendar]] and
    (__ \ "bandwidth").format[Long] and
    (__ \ "reply-to").format[URI] and
    (__ \ "correlation-id").format[CorrelationId] and
    (__ \ "algorithm").format[Option[String]] and
    (__ \ "constraints").format[Seq[String]])((source, destination, start, end, bandwidth, replyTo, correlationId, algorithm, constraints) => {
      val service = new P2PServiceBaseType().withCapacity(bandwidth).withSourceSTP(source).withDestSTP(destination)
      val schedule = new ScheduleType().withStartTime(start.orNull).withEndTime(end.orNull)
      PathComputationRequest(correlationId, replyTo, schedule, service)
    }, {
      case PathComputationRequest(correlationId, replyTo, schedule, service) =>
        (service.getSourceSTP(),
          service.getDestSTP(),
          Option(schedule.getStartTime()),
          Option(schedule.getEndTime()),
          service.getCapacity(),
          replyTo,
          correlationId,
          None,
          Nil)
    })
}
